{-# LANGUAGE CPP #-}
module Database.Schema.Gen where

import Data.List as L
import Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Database.Schema.Def
import Database.Schema.ShowType


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
  "type instance TRelDef " <> sch <> "\n  " <> showType relName <> " = " <> showType rel <> "\n\n"

textTabRel :: Text -> NameNS -> [NameNS] -> [NameNS] -> Text
textTabRel sch tab froms tos
  =  "instance CTabRels " <> pars <> " where\n"
  <> "  type TFrom " <> pars <> " = \n"
  <> "    " <> showSplit 6 70 froms <> "\n"
  <> "  type TTo " <> pars <> " = \n"
  <> "    " <> showSplit 6 70 tos <> "\n"
  where
    pars = T.intercalate " " [sch, showType tab]

-- Build Ref list for a relation (from/to column pairs and FldDefs from mfld).
refsForRel :: RelDef -> M.Map (NameNS, Text) FldDef -> [Ref]
refsForRel rel mfld =
  [ Ref { fromName, toName
        , fromDef = mfld M.! (rdFrom rel, fromName)
        , toDef = mfld M.! (rdTo rel, toName) }
  | (fromName, toName) <- rdCols rel
  ]

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
  let refsText = T.intercalate " " $
        [ textRef (mfld M.! (fromTab, fromName)) (mfld M.! (tab, toName)) fromName toName
        | (fromName, toName) <- rdCols rel
        ]
  in "'RFToHere " <> showType fromTab <> "\n      '[ " <> refsText <> " ]"

rhsFromHere :: NameNS -> NameNS -> RelDef -> M.Map (NameNS, Text) FldDef -> Text
rhsFromHere tab toTab rel mfld =
  let refsText = T.intercalate " " $
        [ textRef (mfld M.! (tab, fromName)) (mfld M.! (toTab, toName)) fromName toName
        | (fromName, toName) <- rdCols rel
        ]
  in "'RFFromHere " <> showType toTab <> "\n      '[ " <> refsText <> " ]"

-- Closed type family TFieldInfo<Sch> and single CFieldInfo instance
typeFamilyName :: Text -> Text
typeFamilyName sch = "TFieldInfo" <> sch

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
  <> "instance CFieldInfo " <> schName <> " t f where\n"
  <> "  type TFieldInfo " <> schName <> " t f = " <> tfName <> " t f\n\n"
  where
    tfName = typeFamilyName schName
      -- All (tab, fldName, rhs) in deterministic order: by table, then plain fields, then toHere, then fromHere
    plainEntries =
      [ (tab, fldName, rhsPlain fd)
      | ((tab, fldName), fd) <- toList mfld
      ]
    toHereEntries =
      [ (tab, nnsName relName, rhsToHere tab (rdFrom rel) rel mfld)
      | (tab, (_, _froms, tos)) <- toList mtab, relName <- tos
      , let rel = mrel M.! relName
      ]
    fromHereEntries =
      [ (tab, nnsName relName, rhsFromHere tab (rdTo rel) rel mfld)
      | (tab, (_, froms, _tos)) <- toList mtab, relName <- froms
      , let rel = mrel M.! relName
      ]
    allEntries = plainEntries <> toHereEntries <> fromHereEntries
    eqnLine tab fldName rhs =
      "  " <> tfName <> " " <> showType tab <> " \"" <> fldName <> "\" = " <> rhs <> "\n"
    perTableDefault _ tabStr fieldNames relNames =
      "  " <> tfName <> " " <> tabStr <> " f = " <> typeErrorMsg schName tabStr fieldNames relNames <> "\n"
    tableBlocks =
      [ mconcat [ eqnLine tab fld rhs | (t, fld, rhs) <- allEntries, t == tab ]
        <> perTableDefault tab (showType tab) (tdFlds td) ((nnsName <$> froms) <> (nnsName <$> tos))
      | (tab, (td, froms, tos)) <- toList mtab ]
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
  <> "import Database.Schema.Def\n"
  <> "import Database.PostgreSQL.Enum\n\n\n"
  <> "data " <> schName <> "\n\n"
  <> mconcat (uncurry (textTypDef schName) <$> toList mtyp)
  <> mconcat ((\(tab,(td,_,_)) -> textTabDef schName tab td) <$> toList mtab)
  <> mconcat ([ textRelDef schName relName rel | (relName, rel) <- toList mrel ])
  <> mconcat ((\(tab,(_,froms,tos)) -> textTabRel schName tab froms tos)
    <$> toList mtab)
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
