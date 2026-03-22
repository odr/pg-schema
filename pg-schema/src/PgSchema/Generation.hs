{-# LANGUAGE CPP #-}
-- |
-- Module: PgSchema.Generation
-- Copyright: (c) Dmitry Olshansky
-- License: BSD-3-Clause
-- Maintainer: olshanskydr@gmail.com, dima@typeable.io
-- Stability: experimental
--
-- === Generation of type-level database schema definitions
--
-- Typically you build an executable that imports this module
-- and run it to emit the schema definition.
--
module PgSchema.Generation
  (updateSchemaFile, GenNames(..), AddRelation(..)
  , NameNS'(..), NameNS, (->>)
  ) where

import Control.Monad
import Control.Monad.Catch
import Data.Bifunctor
import Data.ByteString as BS hiding (readFile, writeFile)
import Data.Coerce
import Data.Functor
import Data.List qualified as L
import Data.List.NonEmpty as NE
import Data.Map as M
import Data.Maybe as Mb
import Data.Set as S
import Data.String
import Data.Text as T
import Data.Text.IO as T
import Data.Traversable
import Database.PostgreSQL.Simple
import GHC.Int
import GHC.Records
import GHC.TypeLits ( Symbol )
import PgSchema.Ann
import PgSchema.DML.Select
import PgSchema.DML.Select.Types
import PgSchema.Schema
import PgSchema.Schema.Catalog
import PgSchema.Schema.Info
import PgSchema.Types
import PgSchema.Utils.ShowType
import Prelude as P
import System.Directory
import System.Environment


data ExceptionSch
  = ConnectException ByteString SomeException
  | GetDataException (Text, [SomeToField]) SomeException
  deriving Show

instance Exception ExceptionSch

data AddRelation = AddRelation
  { name  :: Text
  -- ^ name of an additional (non-existing in the database) relation.
  -- All additional relations will be added with the namespace "_add".
  , from  :: NameNS
  , to    :: NameNS
  , cols  :: [(Text, Text)] }

data GenNames = GenNames
  { schemas :: [Text]   -- ^ generate data for all tables in these schemas
  , tables  :: [NameNS] -- ^ generate data for these tables
  , addRelations :: [AddRelation] -- ^ additional relations. Be careful!
  }

type AnnCat tn = 'Ann RenamerId PgCatalog 3 (PGC tn)

selCat :: forall (tn :: Symbol) -> forall r. (Selectable (AnnCat tn) r)
  => Connection -> QueryParam PgCatalog (PGC tn) -> IO ([r], (Text,[SomeToField]))
selCat tn = selectSch (AnnCat tn)

selTxt :: forall (tn :: Symbol) -> forall r. (Selectable (AnnCat tn) r)
  => QueryParam PgCatalog (PGC tn) -> (Text,[SomeToField])
selTxt tn @r = selectText (AnnCat tn) @r

getSchema
  :: Connection -- ^ connection to PostgreSQL database
  -> GenNames   -- ^ names of schemas and tables to generate from the database
  -> IO ([PgType], [PgClass], [PgRelation])
getSchema conn GenNames {..} = do
  types <- selCat "pg_type" conn qpTyp `catch`
    (throwM . GetDataException (selTxt "pg_type" @PgType qpTyp))
  classes <- L.filter checkClass . fst <$> selCat "pg_class" conn qpClass `catch`
    (throwM . GetDataException (selTxt "pg_class" @PgClass qpClass))
  relations <- L.filter checkRels . (Mb.mapMaybe (mkRel classes) addRelations <>)
    . fst <$> selCat "pg_constraint" conn qpRel `catch`
      (throwM . GetDataException (selTxt "pg_constraint" @PgRelation qpRel))
  pure (fst types, classes, relations)
  where
    mkRel classes ar = do
      conkey <- mkNums ar.from $ fst <$> ar.cols
      confkey <- mkNums ar.to $ snd <$> ar.cols
      pure PgRelation
        { constraint__namespace = "nspname" =: "_add"
        , conname               = ar.name
        , constraint__class     = toPgClassShort ar.from
        , constraint__fclass    = toPgClassShort ar.to
        , .. }
      where
        toPgClassShort nns = PgClassShort
          { class__namespace = "nspname" =: nns.nnsNamespace
          , relname          = nns.nnsName }
        mkNums nns fields = do
          pgcl <- L.find (\c -> c.class__namespace == "nspname" =: nns.nnsNamespace
            && c.relname == nns.nnsName) classes
          inds <- for fields \fld ->
            L.findIndex ((==fld) . (.attname)) pgcl.attribute__class
          pure $ pgArr' $ fromIntegral . (+1) <$> inds

-- all data are ordered to provide stable `hashSchema`
    qpTyp = qRoot @PgCatalog @(PGC "pg_type") do
      qOrderBy [ascf "typname", ordNS "typnamespace"]
      qPath "enum__type" do
        qOrderBy [ascf "enumsortorder"]
    qpClass = qRoot @PgCatalog @(PGC "pg_class") do
      qWhere $ condClass &&& pin "relkind" (PgChar <$> 'v' :| "r") -- views & tables
      qOrderBy [ascf "relname", ordNS "relnamespace"]
      qPath "attribute__class" do
        qWhere $ "attnum" >? (0::Int16)
        qOrderBy [ascf "attnum"]
      qPath "constraint__class" do
        qOrderBy [ascf "conname"]
    qpRel = qRoot @PgCatalog @(PGC "pg_constraint") do
      qWhere
        $   pparent (PGC "constraint__class") condClass
        ||| pparent (PGC "constraint__fclass") condClass
      qOrderBy [ascf "conname", ordNS "connamespace"]
    ordNS fld = UnsafeOrd do
      o <- tabPref
      pure ("(select nspname from pg_catalog.pg_namespace p where p.oid = "
        <> o <> "." <> fld <> ")", Asc)
    condClass = condSchemas ||| condTabs
      where
        condSchemas = pparent (PGC "class__namespace")
          $ foldMap (pin "nspname") $ nonEmpty schemas
        condTabs
          = pparent (PGC "class__namespace")
            (foldMap (pin "nspname" . fmap nnsNamespace) (nonEmpty tables))
          &&& foldMap (pin "relname" . fmap nnsName) (nonEmpty tables)
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
    classAttrs = ((,) <$> tabKey <*> attribute__class) <$> classes
    mClassAttrs =
      M.fromList [((c, attnum a), attname a)| (c,as) <- classAttrs, a <- as]
    attrs :: [(NameNS, PgAttribute)] =
      L.concatMap (\(a,xs) -> (a,) <$> xs) classAttrs
    typKey = NameNS <$> (coerce . type__namespace) <*> typname
    ntypes = ntype <$> L.filter ((`S.member` attrsTypes) . typKey) types
      where
        ntype t = (t, typKey <$> M.lookup (fromPgOid $ typelem t) mtypes)
        attrsTypes = S.fromList $ (typKey . attribute__type . snd <$> attrs)
          <> [pgc "int8", pgc "float8"] -- added for Aggr
        mtypes = M.fromList $ (\x -> (fromPgOid $ oid x , x)) <$> types
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
        ( HasField "class__namespace" r ("nspname" := Text)
        , HasField "relname" r Text )
      => r -> NameNS
    tabKey r = NameNS (coerce r.class__namespace) r.relname
    ptabDef c@PgClass{..} = (tabName, (TabDef{..}, froms, tos))
      where
        tabName = tabKey c
        tdFlds = attname <$> coerce attribute__class
        tdKey = L.concat $ keysBy (=='p')
        tdUniq = keysBy (=='u')
        keysBy f
          = Mb.mapMaybe -- if something is wrong exclude such constraint
          (traverse numToName . unPgArr' . (.conkey))
          $ L.filter (f . coerce . contype) (coerce constraint__class)
          where
            numToName a =
              attname <$> L.find ((==a) . attnum) attribute__class
        (froms, tos) = bimap getNames getNames (rdFrom, rdTo)
          where
            getNames f = fst <$> L.filter ((==tabName) . f . snd) relDefs
    relDefs = Mb.mapMaybe mbRelDef relations
    mbRelDef PgRelation {..} = sequenceA
      ( rdName
      , zipWithM getName2 (unPgArr' conkey) (unPgArr' confkey)
        <&> \rdCols -> RelDef{..})
      where
        rdName = NameNS (coerce constraint__namespace) conname
        rdFrom = tabKey constraint__class
        rdTo = tabKey constraint__fclass
        getName t n = M.lookup (t,n) mClassAttrs
        getName2 n1 n2 = (,) <$> getName rdFrom n1 <*> getName rdTo n2

-- | Update (or create) the Haskell file containing the schema definition
updateSchemaFile
  :: Bool       -- ^ verbose mode
  -> String     -- ^ file name
  -> Either String ByteString
    -- ^ name of environment variable with connection string, or
    -- the connection string itself.
    -- When this environment variable is not set or the connection string is empty,
    -- we do nothing.
  -> Text     -- ^ haskell module name to generate
  -> Text     -- ^ name of generated haskell type for schema
  -> GenNames -- ^ names of schemas in database or tables to generate
  -> IO Bool
updateSchemaFile verbose fileName ecs moduleName schName genNames = do
  connStr <- either getConnStr pure ecs
  if BS.null connStr
    then pure False
    else do
      fe <- doesFileExist fileName
      conn <- connectPostgreSQL connStr
      P.putStrLn "Trying to get schema"
      schema <- getSchema conn genNames
      P.putStrLn "Generation"
      let newTxt = moduleText schema
      needGen <- if fe
        then (/= newTxt) <$> T.readFile fileName
        else pure True
      P.putStrLn $ "Need to generate file: " <> P.show needGen
      when needGen do
        when fe $ copyFile fileName (fileName <> ".bak")
        T.writeFile fileName newTxt
      when verbose $ print schema
      pure needGen
  where
    getConnStr env =
      handle (const @_ @SomeException $ pure "") (fromString <$> getEnv env)
    moduleText = genModuleText moduleName schName . getDefs

mkInst :: ShowType a => Text -> [Text] -> a -> Text
mkInst name pars a
  =  "instance C" <> sgn <> " where\n"
  <> "  type T" <> sgn <> " = \n"
  <> "    " <> showSplit 6 70 a <> "\n"
  where
    sgn = T.intercalate " " (name : pars)

textTypDef :: Text -> NameNS -> TypDef -> Text
textTypDef sch typ td@TypDef {..} = mkInst "TypDef" ss td <> pgEnum
  where
    ss = [sch, showType typ]
    st = T.intercalate " " ss
    pgEnum
      | L.null typEnum = ""
      | otherwise
        = "data instance PGEnum " <> st <> "\n  = "
        <> showSplit' "|" 2 70
          ( T.intercalate " | "
            $ ((T.toTitle (nnsName typ) <> "_") <>) <$> typEnum )
        <> "  deriving (Show, Read, Ord, Eq, Generic, Bounded, Enum)\n\n"
#ifdef MK_HASHABLE
        <> "instance Hashable (PGEnum " <> st <> ")\n\n"
#endif
        <> "instance NFData (PGEnum " <> st <> ")\n\n"

textTabDef :: Text -> NameNS -> TabDef -> Text
textTabDef sch tab = mkInst "TabDef" [sch, showType tab]

textRelDef :: Text -> NameNS -> RelDef -> Text
textRelDef sch relName rel =
  "instance CRelDef " <> sch <> " " <> showType relName <> " where\n" <>
  "  type TRelDef " <> sch <> " " <> showType relName <> " = " <> showType rel <> "\n\n"

textTabRel :: Text -> NameNS -> [NameNS] -> [NameNS] -> Text
textTabRel sch tab froms tos
  =  "instance CTabRels " <> pars <> " where\n"
  <> "  type TFrom " <> pars <> " = \n"
  <> "    " <> showSplit 6 70 froms <> "\n"
  <> "  type TTo " <> pars <> " = \n"
  <> "    " <> showSplit 6 70 tos <> "\n"
  where
    pars = T.intercalate " " [sch, showType tab]

-- Generate Ref in type-level format (using 'FldDef directly)
textRef :: FldDef -> FldDef -> Text -> Text -> Text
textRef fromDef toDef fromName toName =
  "'Ref " <> showType fromName <> " (" <> showType fromDef <> ") "
  <> showType toName <> " (" <> showType toDef <> ")"

-- RHS only (for closed type family equations)
rhsPlain :: FldDef -> Text
rhsPlain fd = "'RFPlain (" <> showType fd <> ")"

rhsToHere :: NameNS -> NameNS -> RelDef -> M.Map (NameNS, Text) FldDef -> Text
rhsToHere tab fromTab rel mfld =
  let refsText = T.intercalate "\n      , " $
        [ textRef (mfld M.! (fromTab, fromName)) (mfld M.! (tab, toName)) fromName toName
        | (fromName, toName) <- rdCols rel
        ]
  in "'RFToHere " <> showType fromTab <> "\n      '[ " <> refsText <> " ]"

rhsFromHere :: NameNS -> NameNS -> RelDef -> M.Map (NameNS, Text) FldDef -> Text
rhsFromHere tab toTab rel mfld =
  let refsText = T.intercalate "\n      , " $
        [ textRef (mfld M.! (tab, fromName)) (mfld M.! (toTab, toName)) fromName toName
        | (fromName, toName) <- rdCols rel
        ]
  in "'RFFromHere " <> showType toTab <> "\n      '[ " <> refsText <> " ]"

rhsSelfRef :: NameNS -> RelDef -> M.Map (NameNS, Text) FldDef -> Text
rhsSelfRef tab rel mfld =
  let refsText = T.intercalate "\n      , " $
        [ textRef (mfld M.! (tab, fromName)) (mfld M.! (tab, toName)) fromName toName
        | (fromName, toName) <- rdCols rel
        ]
  in "'RFSelfRef " <> showType tab <> "\n      '[ " <> refsText <> " ]"

-- Closed type family TDBFieldInfo<Sch> and single CDBFieldInfo instance
typeFamilyName :: Text -> Text
typeFamilyName sch = "TDBFieldInfo" <> sch

typeErrorMsg
  :: Text -> Text -> [Text] -> [Text] -> Text
typeErrorMsg sch tabStr fields rels =
  "TE.TypeError (TE.Text \"In schema \" TE.:<>: TE.ShowType " <> sch
  <> "\n    TE.:$$: TE.Text \"for table \" TE.:<>: TE.ShowType " <> tabStr
  <> "\n    TE.:$$: TE.Text \"name \" TE.:<>: TE.ShowType f TE.:<>: TE.Text \" is not defined.\""
  <> "\n    TE.:$$: TE.Text \"\""
  <> "\n    TE.:$$: TE.Text \"Valid values are:\""
  <> "\n    TE.:$$: TE.Text \"  Fields: " <> T.intercalate ", " fields <> ".\""
  <> "\n    TE.:$$: TE.Text \"  Foreign key constraints: " <> T.intercalate ", " rels <> ".\""
  <> "\n    TE.:$$: TE.Text \"\""
  <> "\n    TE.:$$: TE.Text \"Your source or target type or renaimer is probably invalid.\""
  <> "\n    TE.:$$: TE.Text \"\""
  <> ")"

textClosedFieldInfoTF
  :: Text
  -> (M.Map (NameNS, Text) FldDef
    , M.Map NameNS (TabDef, [NameNS]
    , [NameNS]), M.Map NameNS RelDef)
  -> Text
textClosedFieldInfoTF schName (mfld, mtab, mrel) =
  "type family " <> tfName
  <> " (t :: NameNSK) (f :: TL.Symbol) :: RecFieldK NameNSK where\n" <> equations <> "\n"
  <> "instance (ToStar (TDBFieldInfo " <> schName <> " t f), ToStar t, ToStar f) => CDBFieldInfo " <> schName <> " t f where\n"
  <> "  type TDBFieldInfo " <> schName <> " t f = " <> tfName <> " t f\n\n"
  where
    tfName = typeFamilyName schName
      -- All (tab, fldName, rhs) in deterministic order: by table, then plain fields,
      -- then toHere, then fromHere, then selfRef (for self-FK).
    plainEntries =
      [ (tab, fldName, rhsPlain fd) | ((tab, fldName), fd) <- M.toList mfld ]
    toHereEntries =
      [ (tab, nnsName relName, rhsToHere tab (rdFrom rel) rel mfld)
      | (tab, (_, _froms, tos)) <- M.toList mtab, relName <- tos
      , let rel = mrel M.! relName
      , not (rdFrom rel == tab && rdTo rel == tab)
      ]
    fromHereEntries =
      [ (tab, nnsName relName, rhsFromHere tab (rdTo rel) rel mfld)
      | (tab, (_, froms, _tos)) <- M.toList mtab, relName <- froms
      , let rel = mrel M.! relName
      , not (rdFrom rel == tab && rdTo rel == tab)
      ]
    selfEntries =
      [ (tab, nnsName relName, rhsSelfRef tab rel mfld)
      | (tab, (_, froms, tos)) <- M.toList mtab
      , relName <- L.nub (tos <> froms)
      , let rel = mrel M.! relName
      , rdFrom rel == tab && rdTo rel == tab
      ]
    allEntries = plainEntries <> toHereEntries <> fromHereEntries <> selfEntries
    eqnLine tab fldName rhs =
      "  " <> tfName <> " " <> showType tab <> " \"" <> fldName <> "\" = " <> rhs <> "\n"
    perTableDefault _ tabStr fieldNames relNames =
      "  " <> tfName <> " " <> tabStr <> " f = " <> typeErrorMsg schName tabStr fieldNames relNames <> "\n"
    tableBlocks =
      [ mconcat [ eqnLine tab fld rhs | (t, fld, rhs) <- allEntries, t == tab ]
        <> perTableDefault tab (showType tab) (tdFlds td) ((nnsName <$> froms) <> (nnsName <$> tos))
      | (tab, (td, froms, tos)) <- M.toList mtab ]
    equations = mconcat tableBlocks <> "  " <> tfName <> " t f = "
      <> typeErrorMsgFinal <> "\n"
    typeErrorMsgFinal =
      "TE.TypeError (TE.Text \"In schema \" TE.:<>: TE.ShowType " <> schName
      <> " TE.:<>: TE.Text \" the table \" TE.:<>: TE.ShowType t TE.:<>: TE.Text \" is not defined.\""
      <> "\n    TE.:$$: TE.Text \"\""
      <> ")"


genModuleText
  :: Text -- ^ module name
  -> Text -- ^ schema name
  -> (Map NameNS TypDef
    , Map (NameNS,Text) FldDef
    , Map NameNS (TabDef, [NameNS], [NameNS])
    , Map NameNS RelDef)
  -> Text
genModuleText moduleName schName (mtyp, mfld, mtab, mrel)
  =  "{- HLINT ignore -}\n"
  <> "{-# LANGUAGE FlexibleContexts #-}\n"
  <> "{-# LANGUAGE TypeFamilies #-}\n"
  <> "{-# LANGUAGE UndecidableInstances #-}\n"
  <> "{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}\n"
  <> "{-# OPTIONS_GHC -fno-warn-unused-imports #-}\n"
  <> "{-# OPTIONS_GHC -freduction-depth=300 #-}\n"
  <> "module " <> moduleName <> " where\n\n"
  <> "-- This file is generated and can't be edited.\n\n"
  <> "import Control.DeepSeq\n" -- for PGEnum if exist
#ifdef MK_HASHABLE
  <> "import Data.Hashable\n" -- for PGEnum if exist
#endif
  <> "import GHC.Generics\n" -- for PGEnum if exists
  <> "import GHC.TypeError qualified as TE\n"
  <> "import GHC.TypeLits qualified as TL\n"
  <> "import PgSchema.Import\n"
  <> "data " <> schName <> "\n\n"
  <> mconcat (uncurry (textTypDef schName) <$> M.toList mtyp)
  <> mconcat ((\(tab,(td,_,_)) -> textTabDef schName tab td) <$> M.toList mtab)
  <> mconcat ([ textRelDef schName relName rel | (relName, rel) <- M.toList mrel ])
  <> mconcat ((\(tab,(_,froms,tos)) -> textTabRel schName tab froms tos)
    <$> M.toList mtab)
  <> textClosedFieldInfoTF schName (mfld, mtab, mrel)
  <> "instance CSchema " <> schName <> " where\n"
  <> "  type TTabs " <> schName <> " = " <> showSplit 4 70 (keys mtab) <> "\n"
  <> "  type TTypes " <> schName <> " = " <> showSplit 4 70 (keys mtyp) <> "\n"

showSplit :: ShowType a => Int -> Int -> a -> Text
showSplit shift width = showSplit' "," shift width . showType

showSplit' :: Text -> Int -> Int -> Text -> Text
showSplit' delim shift width
  = T.unlines . mapTail ((T.replicate shift " " <>) . (delim <>))
  . L.map (T.intercalate delim) . fst . mkLines
  where
    mapTail _ []     = []
    mapTail f (x:xs) = x : L.map f xs
    mkLines = L.foldr step ([],0). T.splitOn delim
      where
        step t (xs,len)
          | tlen + len > width = ([t] : xs, tlen)
          | otherwise = case xs of
            []     -> ([[t]], tlen)
            z : zs -> ((t:z):zs, tlen + len)
          where
            tlen = T.length t
