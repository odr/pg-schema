module Database.PostgreSQL.Schema.TH where

import Control.Monad.Catch
import Control.Monad.Zip
import Data.Bifunctor
import Data.ByteString as BS
import Data.Coerce
import Data.List as L
import Data.Map as M
import Data.Maybe as Mb
import Data.Semigroup ((<>))
import Data.Set as S
import Data.Text as T
import Database.PostgreSQL.Convert
import Database.PostgreSQL.DML.Condition
import Database.PostgreSQL.DML.Order
import Database.PostgreSQL.DML.Select
import Database.PostgreSQL.Enum
import Database.PostgreSQL.PgTagged
import Database.PostgreSQL.Schema.Catalog
import Database.PostgreSQL.Schema.Info
import Database.PostgreSQL.Simple
import Database.Schema.Def
import Database.Schema.TH
import Database.Types.SchList
import GHC.Generics
import Language.Haskell.TH


data ExceptionSch
  = ConnectException ByteString SomeException
  | GetDataException (Text, [SomeToField]) SomeException
  deriving Show

instance Exception ExceptionSch

getSchema :: Connection -> Text -> IO ([PgType], [PgClass], [PgRelation])
getSchema conn ns = do
  types <- catch (selectSch @PgCatalog @"pg_type" @PgType conn qpEmpty)
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
    qpClass = qpEmpty
      { qpConds =
        [ rootCond
          $ pparent @"class__namespace" (#nspname =? ns)
            &&& (pin @"relkind" (PgChar <$> "vr"))
        , cwp @'["attribute__class"] (#attnum >? (0::Int)) ]
      , qpOrds =
        [ owp @'["attribute__class"] @'[ '("attnum",'Asc)] ] }
    qpRel = qpEmpty
      { qpConds =
        [rootCond $ pparent @"constraint__namespace" (#nspname =? ns)] }

mkSchema :: ByteString -> Name -> Text -> DecsQ
mkSchema connStr sch ns = do
  (types, classes, relations) <- runIO $ do
    conn <- catch (connectPostgreSQL connStr) (throwM . ConnectException connStr)
    getSchema conn ns

  let
    classAttrs = ((,) <$> relname <*> getSchList . attribute__class) <$> classes
    mClassAttrs =
      M.fromList [((c, attnum a), attname a)| (c,as) <- classAttrs, a <- as]
    attrs = L.concat $ (\(a,xs) -> (a,) <$> xs) <$> classAttrs
    attrsTypes = S.fromList $ (unPgTag . attribute__type . snd) <$> attrs
    mtypes = M.fromList . fmap (\x -> (oid x, x)) $ types
    ntypes = (\t -> (t, T.unpack . typname <$> M.lookup (typelem t) mtypes))
      <$> L.filter ((`S.member` attrsTypes) . typname) types

  -- reportWarning $ "ntypes count: " ++ show (L.length ntypes)
  -- reportWarning $ "attrs count: " ++ show (L.length attrs)
  typs <- L.concat <$> traverse instTypDef ntypes
  flds <- L.concat <$> traverse instFldDef attrs
  tabs <- L.concat <$> traverse instTabDef classes
  rls <- fmap L.concat . traverse instRelDef
    $ Mb.mapMaybe (mkRelDef mClassAttrs) relations
  schema <- instSchema (relname <$> classes)
    ((\PgRelation {..} -> conname) <$> relations)
    (typname . fst <$> ntypes)
  pure $ typs ++ flds ++ tabs ++ rls ++ schema
  where
    schQ = conT sch
    instTypDef (pgt,mbn) = (++)
      <$> [d|
        instance CTypDef $(schQ) $(nameQ) where
          type TTypDef $(schQ) $(nameQ) =
            'TypDef $(categoryQ) $(typElemQ) $(enumQ)
        |]
      <*> if L.null enumL then pure [] else
        (sequence
          [dataInstD (pure []) ''PGEnum [schQ, nameQ] Nothing enumsQ
            [ derivClause Nothing
              [[t|Show|], [t|Read|], [t|Ord|], [t|Eq|], [t|Generic|]] ] ])
      where
        enumL = getSchList $ enumlabel <$> enum__type pgt
        nameQ = pure $ txtToSym $ typname pgt
        categoryQ = pure $ strToSym [coerce $ typcategory pgt]
        enumQ = pure $ toPromotedList $ txtToSym <$> enumL
        typElemQ = toPromotedMaybeQ $ strToSym <$> mbn
        enumsQ = flip normalC [] . mkName . T.unpack
          . ((toTitle (typname pgt) <> "_") <>) <$> enumL
    instFldDef (cname, attr) = [d|
      instance CFldDef $(schQ) $(tabQ) $(fldQ) where
        type TFldDef $(schQ) $(tabQ) $(fldQ) =
          'FldDef $(typQ) $(nulQ) $(defQ)
      |]
      where
        tabQ = pure $ txtToSym cname
        fldQ = pure $ txtToSym $ attname attr
        typQ = pure $ txtToSym $ unPgTag $ attribute__type attr
        nulQ = boolQ $ not $ attnotnull attr
        defQ = boolQ $ atthasdef attr
    instTabDef PgClass {..} = [d|
      instance CTabDef $(schQ) $(tabQ) where
        type TTabDef $(schQ) $(tabQ) =
          'TabDef $(fsQ) $(pkQ) $(ukQ)
      |]
      where
        tabQ = pure $ txtToSym relname
        attrs = getSchList attribute__class
        constrs = getSchList constraint__class
        fsQ = pure . toPromotedList $ txtToSym . attname <$> attrs
        numToSym a =
          txtToSym . attname <$> L.find ((==a) . attnum) attrs
        keysBy f
          = catMaybes -- if something is wrong exclude such constraint
          $ traverse numToSym . (\PgConstraint {..} -> getPgArr conkey)
          <$> L.filter (f . coerce . contype) constrs
        pkQ = pure . toPromotedList . L.concat $ keysBy (=='p')
        ukQ = pure . toPromotedList $ toPromotedList <$> keysBy (=='u')
    mkRelDef mClassAttrs PgRelation {..} = sequenceA
      ( conname
      , RelDef
        <$> pure fromName
        <*> pure toName
        <*> sequenceA (getPgArr $ mzipWith getName2 conkey confkey) )
      where
        fromName = unPgTag constraint__class
        toName = unPgTag constraint__fclass
        getName t n = M.lookup (t,n) mClassAttrs
        getName2 n1 n2 = (,) <$> getName fromName n1 <*> getName toName n2
    instRelDef (c, RelDef {..}) = [d|
      instance CRelDef $(schQ) $(relQ) where
        type TRelDef $(schQ) $(relQ) =
          'RelDef $(fromQ) $(toQ) $(colsQ)
      |]
      where
        relQ = pure $ txtToSym c
        fromQ = pure $ txtToSym rdFrom
        toQ = pure $ txtToSym rdTo
        colsQ
          = fmap toPromotedList . traverse pairQ
          $ bimap txtToSym txtToSym <$> rdCols

    instSchema ts rs typs = [d|
      instance CSchema $(schQ) where
        type TSchema $(schQ) = $(pure $ txtToSym ns)
        type TTabs $(schQ) = $(pure $ toPromotedList $ L.map txtToSym ts)
        type TRels $(schQ) = $(pure $ toPromotedList $ L.map txtToSym rs)
        type TTypes $(schQ) = $(pure $ toPromotedList $ L.map txtToSym typs)
      |]

    -- class CSchema sch where
    --   type TSchema sch  :: Symbol
    --   type TTabs sch    :: [Symbol]
    --   type TRels sch    :: [Symbol]

    --
    -- data RelDef' s = RelDef
    --   { rdFrom    :: s
    --   , rdTo      :: s
    --   , rdCols    :: [(s,s)]
    --   , rdDelCons :: DelCons }

toPromotedMaybeQ :: Maybe Type -> TypeQ
toPromotedMaybeQ Nothing  = [t|'Nothing|]
toPromotedMaybeQ (Just t) = appT [t|'Just|] (pure t)

boolQ :: Bool -> TypeQ
boolQ True  = [t|'True|]
boolQ False = [t|'False|]

pairQ :: (Type,Type) -> TypeQ
pairQ (a,b) = [t| '( $(pure a), $(pure b) )|]
