{-| Helper module: export a schema as DOT (Graphviz-style graphs). Useful for
debugging and documenting a schema; not the primary API for database work through
@pg-schema@.

It is exposed as a small utility; it could be made non-public in a future release.
Avoid depending on it in compatibility-sensitive code unless you really need graph output.
-}
module PgSchema.Utils.GenDot where

import Data.Foldable as F
import Data.List as L
import Data.Map as M
import Data.Singletons
import Data.Text as T

import PgSchema.Schema


data DotOper
  = ExcludeToTab NameNS Text
  -- ^ Exclude relation to given table from DOT representation.

genDot :: forall sch. CSchema sch
  => Bool -- ^ Whether to use qualified names
  -> [DotOper] -- ^ Operations to exclude relations from DOT representation
  -> Text
genDot isQual dos = F.fold
  [ "digraph G {\n"
  , "  penwidth=2\n\n"
  , F.fold dbSchemas
  , ""
  , T.unlines relTxts
  , "}\n"
  ]
  where
    tabs = demote @(TTabs sch)
    tim = tabInfoMap @sch
    rels = L.concatMap elems . elems $ tiFrom <$> tim
    tabsByName = M.fromListWith (<>)
      $ ((,) <$> nnsName <*> pure . nnsNamespace) <$> tabs
    qName nns
      | isQual    = q
      | otherwise = case M.lookup (nnsName nns) tabsByName of
        Just [_] -> (nnsName nns,False)
        _        -> q
      where
        q = (qualName nns,True)
    qNameQuo nns = case qName nns of
      (x,False) -> x
      (x,True)  -> "\"" <> x <> "\""
    qName' nns = case exTo of
      [] -> nns'
      _  -> nns' <> " [label=\"" <> fst (qName nns) <> ": "
        <> T.unwords exTo <> "\"]"
      where
        nns' = qNameQuo nns
        exTo = [t | ExcludeToTab x t <- dos, RelDef {..} <- rels
          , rdTo == x, rdFrom == nns]
    dbSchemas = dbSchema <$> nub (nnsNamespace <$> tabs)
    dbSchema schName = F.fold
      [ "  subgraph cluster_" <> schName <> "{\n"
      , "    label=\"" <> schName <> "\"\n"
      , T.unlines
        $ ("    " <>) . qName' <$> L.filter ((==schName) . nnsNamespace) tabs
      , "  }\n"
      ]
    relTxts = rel <$> L.filter notExcluded rels
      where
        notExcluded RelDef{..} = L.null [()|ExcludeToTab x _ <- dos, rdTo ==x]
    rel RelDef {..} = "  " <> qNameQuo rdFrom <> "->" <> qNameQuo rdTo <> clr
      where
        clr = case mbTabFlds >>= isNullableRef of
          Just True -> "[color=\"green\"]"
          _         -> ""
          where
            mbTabFlds = tiFlds <$> M.lookup rdFrom tim
            isNullableRef tabFlds = L.any fdNullable
              <$> traverse ((`M.lookup` tabFlds) . fst) rdCols
