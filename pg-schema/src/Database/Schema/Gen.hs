module Database.Schema.Gen where

import Data.List as L
import Data.Map as M
import Data.Semigroup ((<>))
import Data.String
import Data.Text as T
import Database.Schema.Def
import Util.ShowType


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
            $ ((toTitle (nnsName typ) <> "_") <>) <$> typEnum )
        <> "\n"
        <> "  deriving (Show, Read, Ord, Eq, Generic)\n\n"
        <> "instance NFData (PGEnum " <> st <> ")\n\n"

textFldDef :: Text -> NameNS -> Text -> FldDef -> Text
textFldDef sch tab fld =
  mkInst "FldDef" [sch, showType tab, showType fld]

textTabDef :: Text -> NameNS -> TabDef -> Text
textTabDef sch tab = mkInst "TabDef" [sch, showType tab]

textRelDef :: Text -> NameNS -> RelDef -> Text
textRelDef sch rel = mkInst "RelDef" [sch, showType rel]

textTabRel :: Text -> NameNS -> [NameNS] -> [NameNS] -> Text
textTabRel sch tab froms tos
  =  "instance CTabRels " <> pars <> " where\n"
  <> "  type TFrom " <> pars <> " = \n"
  <> "    " <> showSplit 6 70 froms <> "\n"
  <> "  type TTo " <> pars <> " = \n"
  <> "    " <> showSplit 6 70 tos <> "\n"
  where
    pars = T.intercalate " " [sch, showType tab]

genModuleText
  :: Text -- ^ module name
  -> Text -- ^ schema name
  -> Int  -- ^ schema hash value
  -> (Map NameNS TypDef
    , Map (NameNS,Text) FldDef
    , Map NameNS (TabDef, [NameNS], [NameNS])
    , Map NameNS RelDef)
  -> Text
genModuleText moduleName schName hash (mtyp, mfld, mtab, mrel)
  =  "{- HLINT ignore -}\n"
  <> "{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}\n"
  <> "{-# OPTIONS_GHC -fno-warn-unused-imports #-}\n"
  <> "{-# OPTIONS_GHC -freduction-depth=300 #-}\n"
  <> "module " <> moduleName <> " where\n\n"
  <> "-- This file is generated and can't be edited.\n\n"
  <> "import Control.DeepSeq\n" -- for PGEnum if exist
  <> "import GHC.Generics\n" -- for PGEnum if exists
  <> "import PgSchema\n\n\n"
  <> "hashSchema :: Int\n"
  <> "hashSchema = " <> fromString (show hash) <> "\n\n"
  <> "data " <> schName <> "\n\n"
  <> mconcat (uncurry (textTypDef schName) <$> toList mtyp)
  <> mconcat ((\((a,b),c) -> textFldDef schName a b c) <$> toList mfld)
  <> mconcat ((\(tab,(td,_,_)) -> textTabDef schName tab td) <$> toList mtab)
  <> mconcat (L.map (uncurry $ textRelDef schName) $ toList mrel)
  <> mconcat ((\(tab,(_,froms,tos)) -> textTabRel schName tab froms tos)
    <$> toList mtab)
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
