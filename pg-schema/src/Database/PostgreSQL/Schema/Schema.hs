module Database.PostgreSQL.Schema.Schema where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Zip
import Data.Bifunctor
import Data.ByteString as BS hiding (readFile, writeFile)
import Data.Coerce
import Data.Hashable
import Data.List as L
import Data.List.NonEmpty as NE
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
import GHC.Int
import GHC.Records
import System.Directory
import System.Environment


data ExceptionSch
  = ConnectException ByteString SomeException
  | GetDataException (Text, [SomeToField]) SomeException
  deriving Show

instance Exception ExceptionSch

data GenNames = GenNames
  { schemas :: [Text]   -- ^ generate data for all tables in these schemas
  , tables  :: [NameNS] -- ^ generate data for these tables
  }

getSchema
  :: Connection -- ^ connection to PostgreSQL database
  -> GenNames   -- ^ names of schemas in database or tables to generate
  -> IO ([PgType], [PgClass], [PgRelation])
getSchema conn GenNames {..} = do
  types <- catch (selectSch @PgCatalog @(PGC "pg_type") @PgType conn qpTyp)
    ( throwM . GetDataException
      (selectText @PgCatalog @(PGC "pg_type") @PgType qpEmpty ) )
  classes <- L.filter checkClass <$>
    catch (selectSch @PgCatalog @(PGC "pg_class") @PgClass conn qpClass)
      ( throwM . GetDataException
        (selectText @PgCatalog @(PGC "pg_class") @PgClass qpClass) )
  relations <- L.filter checkRels <$>
    catch (selectSch @PgCatalog @(PGC "pg_constraint") @PgRelation conn qpRel)
      ( throwM . GetDataException
        (selectText @PgCatalog @(PGC "pg_constraint") @PgRelation qpRel) )
  pure (types, classes, relations)
  where
    -- all data are ordered to provide stable `hashSchema`
    qpTyp = qpEmpty
      { qpOrds =
        [ rootOrd [ascf @"typname"]
        , owp @'["enum__type"] [ascf @"enumsortorder"] ] }
    qpClass = qpEmpty
      { qpConds =
        [ rootCond $ condClass
          &&& pin @"relkind" (PgChar <$> 'v' :| "r") -- views & tables
        , cwp @'["attribute__class"] (fld @"attnum" >? (0::Int16)) ]
      , qpOrds =
        [ rootOrd [ascf @"relname"]
        , owp @'["attribute__class"] [ascf @"attnum"]
        , owp @'["constraint__class"] [ascf @"conname"] ] }
    condClass = condSchemas ||| condTabs
      where
        condSchemas = pparent @(PGC "class__namespace")
          $ foldMap (pin @"nspname") $ nonEmpty schemas
        condTabs
          = pparent @(PGC "class__namespace")
            (foldMap (pin @"nspname" . fmap nnsNamespace) (nonEmpty tables))
          &&& foldMap (pin @"relname" . fmap nnsName) (nonEmpty tables)
    qpRel = qpEmpty
      { qpConds = [rootCond condRels]
      , qpOrds = [ rootOrd [ascf @"conname"] ] }
      where
        condRels
          = pparent @(PGC "constraint__class") condClass
          ||| pparent @(PGC "constraint__fclass") condClass
    checkClass PgClass {..}
      = (coerce class__namespace `L.elem` schemas)
      || (coerce class__namespace ->> relname `L.elem` tables)
    checkRels PgRelation {..} =
      check constraint__class || check constraint__fclass
      where
        check PgClassShort {..}
          = (coerce class__namespace `L.elem` schemas)
          || (coerce class__namespace ->> relname `L.elem` tables)

getDefs
  :: ([PgType], [PgClass], [PgRelation])
  -> (Map NameNS TypDef
    , Map (NameNS,Text) FldDef
    , Map NameNS (TabDef, [NameNS], [NameNS])
    , Map NameNS RelDef)
getDefs (types,classes,relations) =
  ( M.fromList $ ptypDef <$> ntypes
  , M.fromList $ pfldDef <$> attrs
  , M.fromList $ ptabDef <$> classes
  , M.fromList relDefs )
  where
    classAttrs = ((,) <$> tabKey <*> getSchList . attribute__class) <$> classes
    mClassAttrs =
      M.fromList [((c, attnum a), attname a)| (c,as) <- classAttrs, a <- as]
    attrs :: [(NameNS, PgAttribute)] =
      L.concatMap (\(a,xs) -> (a,) <$> xs) classAttrs
    typKey = NameNS <$> (coerce . type__namespace) <*> typname
    ntypes = ntype <$> L.filter ((`S.member` attrsTypes) . typKey) types
      where
        ntype t = (t, typKey <$> M.lookup (typelem t) mtypes)
        attrsTypes = S.fromList $ typKey . attribute__type . snd <$> attrs
        mtypes = M.fromList $ (\x -> (oid x , x)) <$> types
    ptypDef (x@PgType{..}, typElem) = (typKey x, TypDef {..})
      where
        typCategory = T.singleton $ coerce typcategory
        typEnum = enumlabel <$> coerce enum__type
    pfldDef (cname::NameNS, PgAttribute{..}) = ((cname,attname), FldDef{..})
      where
        fdType = typKey attribute__type
        fdNullable = not attnotnull
        fdHasDefault = atthasdef
    tabKey
      :: forall r .
        ( HasField "class__namespace" r (PgTagged "nspname" Text)
        , HasField "relname" r Text )
      => r -> NameNS
    tabKey r =
      NameNS (coerce $ getField @"class__namespace" r) (getField @"relname" r)
    ptabDef c@PgClass{..} = (tabName, (TabDef{..}, froms, tos))
      where
        tabName = tabKey c
        tdFlds = attname <$> coerce attribute__class
        tdKey = L.concat $ keysBy (=='p')
        tdUniq = keysBy (=='u')
        keysBy f
          = Mb.mapMaybe -- if something is wrong exclude such constraint
          (traverse numToName . coerce . (\PgConstraint {..} -> conkey))
          $ L.filter (f . coerce . contype) (coerce constraint__class)
          where
            numToName a =
              attname <$> L.find ((==a) . attnum) (getSchList attribute__class)
        (froms, tos) = bimap getNames getNames (rdFrom, rdTo)
          where
            getNames f = fst <$> L.filter ((==tabName) . f . snd) relDefs
    relDefs = Mb.mapMaybe mbRelDef relations
    mbRelDef PgRelation {..} = sequenceA
      ( NameNS (coerce constraint__namespace) conname
      , RelDef fromName toName
        <$> sequenceA (coerce $ mzipWith getName2 conkey confkey) )
      where
        fromName = tabKey constraint__class
        toName = tabKey constraint__fclass
        getName t n = M.lookup (t,n) mClassAttrs
        getName2 n1 n2 = (,) <$> getName fromName n1 <*> getName toName n2

updateSchemaFile
  :: String     -- ^ file name
  -> Either String ByteString
    -- ^ name of environment variable with connect string or
    -- connect string as is.
    -- When this environment variable is not set or connect string is empty,
    -- we do nothing.
  -> Text     -- ^ haskell module name to generate
  -> Text     -- ^ name of generated haskell type for schema
  -> GenNames -- ^ names of schemas in database or tables to generate
  -> IO ()
updateSchemaFile fileName ecs moduleName schName genNames = do
  connStr <- either getConnStr pure ecs
  unless (BS.null connStr) $ do
    fe <- doesFileExist fileName
    conn <- connectPostgreSQL connStr
    (schema,h) <- ((,) <$> id <*> hash) <$> getSchema conn genNames
    needGen <- if fe
      then do
        mbhs
          <- L.find ((== ["hashSchema","="]) . L.take 2) . L.map T.words . lines'
          <$> T.readFile fileName
        pure $ case mbhs of
          Just [_,_,x] | x == fromString (show h) -> False
          _                                       -> True
      else pure True
    when needGen $ T.writeFile fileName $ moduleText h schema
  where
    getConnStr env =
      handle (const @_ @SomeException $ pure "") (fromString <$> getEnv env)
    moduleText h = genModuleText moduleName schName h . getDefs
    -- for eager file read
    lines' s
      | T.length s == 0 = []
      | otherwise = T.lines s
