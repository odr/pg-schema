module Database.PostgreSQL.Schema.Schema where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Zip
import Data.ByteString as BS hiding (readFile, writeFile)
import Data.Coerce
import Data.Hashable
import Data.List as L
import Data.Map as M
import Data.Maybe as Mb
import Data.Set as S
import Data.String
import Data.Tagged
import Data.Text as T
import Data.Text.IO as T
import Database.PostgreSQL.Convert
import Database.PostgreSQL.DML.Condition
import Database.PostgreSQL.DML.Order
import Database.PostgreSQL.DML.Select
import Database.PostgreSQL.PgTagged
import Database.PostgreSQL.Schema.Catalog
import Database.PostgreSQL.Schema.Info
import Database.PostgreSQL.Simple
import Database.Schema.Def
import Database.Schema.Gen
import Database.Types.SchList
import Debug.Trace
import System.Directory
import System.Environment


data ExceptionSch
  = ConnectException ByteString SomeException
  | GetDataException (Text, [SomeToField]) SomeException
  deriving Show

instance Exception ExceptionSch

getSchema
  :: Connection -- ^ connection to PostgreSQL database
  -> Text       -- ^ db-schema name
  -> IO ([PgType], [PgClass], [PgRelation])
getSchema conn ns = do
  types <- catch (selectSch @PgCatalog @"pg_type" @PgType conn qpTyp)
    ( throwM . GetDataException
      (selectText @PgCatalog @"pg_type" @PgType qpEmpty ) )
  classes <- catch (selectSch @PgCatalog @"pg_class" @PgClass conn qpClass)
    ( throwM . GetDataException
      (selectText @PgCatalog @"pg_class" @PgClass qpClass) )
  relations <- catch
    (selectSch @PgCatalog @"pg_constraint" @PgRelation conn qpRel)
    ( throwM . GetDataException
      (selectText @PgCatalog @"pg_constraint" @PgRelation qpRel) )
  pure (types, classes, relations)
  where
    -- all data are ordered to provide stable `hashSchema`
    qpTyp = qpEmpty
      { qpOrds =
        [ rootOrd [ascf @"typname"]
        , owp @'["enum__type"] [ascf @"enumsortorder"] ] }
    qpClass = qpEmpty
      { qpConds =
        [ rootCond
          $ pparent @"class__namespace" (#nspname =? ns)
            &&& (pin @"relkind" (PgChar <$> "vr")) -- views & tables
        , cwp @'["attribute__class"] (#attnum >? (0::Int)) ]
      , qpOrds =
        [ rootOrd [ascf @"relname"]
        , owp @'["attribute__class"] [ascf @"attnum"]
        , owp @'["constraint__class"] [ascf @"conname"] ] }
    qpRel = qpEmpty
      { qpConds =
        [rootCond $ pparent @"constraint__namespace" (#nspname =? ns)]
      , qpOrds = [ rootOrd [ascf @"conname"] ] }

getDefs
  :: ([PgType], [PgClass], [PgRelation])
  -> (Map Text TypDef, Map (Text,Text) FldDef, Map Text TabDef, Map Text RelDef)
getDefs (types,classes,relations) =
  ( M.fromList $ ptypDef <$> ntypes
  , M.fromList $ pfldDef <$> attrs
  , M.fromList $ ptabDef <$> classes
  , M.fromList $ Mb.mapMaybe mbRelDef relations )
  where
    classAttrs = ((,) <$> relname <*> getSchList . attribute__class) <$> classes
    mClassAttrs =
      M.fromList [((c, attnum a), attname a)| (c,as) <- classAttrs, a <- as]
    attrs = L.concat $ (\(a,xs) -> (a,) <$> xs) <$> classAttrs
    ntypes = (\t -> (t, typname <$> M.lookup (typelem t) mtypes))
      <$> L.filter ((`S.member` attrsTypes) . typname) types
      where
        attrsTypes = S.fromList $ (unPgTag . attribute__type . snd) <$> attrs
        mtypes = M.fromList . fmap (\x -> (oid x, x)) $ types
    ptypDef (PgType{..}, typElem) = (typname, TypDef {..})
      where
        typCategory = T.singleton $ coerce typcategory
        typEnum = enumlabel <$> coerce enum__type
    pfldDef (cname, PgAttribute{..}) = ((cname,attname), FldDef{..})
      where
        fdType = coerce attribute__type
        fdNullable = not attnotnull
        fdHasDefault = atthasdef
    ptabDef PgClass{..} = (relname, TabDef{..})
      where
        tdFlds = attname <$> coerce attribute__class
        tdKey = L.concat $ keysBy (=='p')
        tdUniq = keysBy (=='u')
        keysBy f
          = catMaybes -- if something is wrong exclude such constraint
          $ traverse numToName . coerce . (\PgConstraint {..} -> conkey)
          <$> L.filter (f . coerce . contype) (coerce constraint__class)
          where
            numToName a =
              attname <$> L.find ((==a) . attnum) (getSchList attribute__class)
    mbRelDef PgRelation {..} = sequenceA
      ( conname
      , RelDef
        <$> pure fromName
        <*> pure toName
        <*> sequenceA (coerce $ mzipWith getName2 conkey confkey) )
      where
        fromName = unPgTag constraint__class
        toName = unPgTag constraint__fclass
        getName t n = M.lookup (t,n) mClassAttrs
        getName2 n1 n2 = (,) <$> getName fromName n1 <*> getName toName n2

{-
getSchemaHash :: Connection -> Text -> IO Int
getSchemaHash conn = fmap hash . getSchema conn

getSchemaHash' :: ByteString -> Text -> IO Int
getSchemaHash' connStr ns = connectPostgreSQL connStr >>= flip getSchemaHash ns

updateSchemaHash :: ByteString -> Text -> FilePath -> IO Bool
updateSchemaHash connStr dbSchema file = do
  h <- getSchemaHash' connStr dbSchema
  s <- readFile file
  let
    s' = L.unlines $ setHash h <$> L.lines s
    -- check length to force close file. Where indirective...
    isChanged = Prelude.length s > 0 && s /= s'
  when isChanged $ writeFile file s'
  pure isChanged
  where
    setHash h s = case L.words s of
      ["hashSchema","=",x]
        | (fst <$> reads x) /= [h]  -> "hashSchema = " <> show h
      _                             -> s
-}
updateSchemaFile
  :: String     -- ^ file name
  -> Either String ByteString
    -- ^ name of environment variable with connect string or
    -- connect string as is.
    -- When this environment variable is not set or connect string is empty,
    -- we generate nothing.
  -> Text       -- ^ haskell module name to generate
  -> Text       -- ^ name of generated haskell type for schema
  -> Text       -- ^ name of schema in database
  -> IO ()
updateSchemaFile fileName ecs moduleName schName dbSchemaName = trace "updateSchemaFile" $ do
  connStr <- either getConnStr pure ecs
  unless (BS.null connStr) $ do
    fe <- doesFileExist fileName
    conn <- connectPostgreSQL connStr
    (schema,h) <- ((,) <$> id <*> hash) <$> getSchema conn dbSchemaName
    needGen <- if fe
      then do
        mbhs
          <- L.find ((== ["hashSchema","="]) . L.take 2) . L.map T.words . lines'
          <$> T.readFile fileName
        pure $ case mbhs of
          Just [_,_,x] | x == fromString (show h) -> False
          _            -> True
      else (pure True)
    trace "needGen" $ when needGen $ trace "writeFile" $ T.writeFile fileName $ moduleText h schema
  where
    getConnStr env =
      handle (const @_ @SomeException $ pure "") (fromString <$> getEnv env)
    moduleText h = genModuleText moduleName schName dbSchemaName h . getDefs
    -- for eager file read
    lines' s
      | T.length s == 0 = []
      | otherwise = T.lines s