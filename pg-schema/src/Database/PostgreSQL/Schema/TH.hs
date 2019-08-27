module Database.PostgreSQL.Schema.TH where

import Control.Monad.Catch
import Data.ByteString as BS
import Data.List as L
import Data.Map as M
import Data.Semigroup ((<>))
import Data.Text as T
import Database.PostgreSQL.Enum
import Database.PostgreSQL.Schema.Schema
import Database.PostgreSQL.Simple
import Database.Schema.Def
import GHC.Generics
import Language.Haskell.TH
import Util.TH.LiftType


thTypDef :: Name -> NameNS -> TypDef -> DecsQ
thTypDef sch name td@(TypDef{..}) = (++)
  <$> [d|
  instance CTypDef $(liftType sch) $(liftType name) where
    type TTypDef $(liftType sch) $(liftType name) = $(liftType td)
  |]
  <*> if L.null typEnum then pure [] else
    sequence
      [ dataInstD (pure []) ''PGEnum [liftType sch, liftType name] Nothing
        enumsQ
        [ derivClause Nothing
          [[t|Show|], [t|Read|], [t|Ord|], [t|Eq|], [t|Generic|]] ] ]
  where
    enumsQ
      = flip normalC [] . mkName . T.unpack
      . ((toTitle (nnsName name) <> "_") <>)
      <$> typEnum

thFldDef :: Name -> NameNS -> Text -> FldDef -> DecsQ
thFldDef sch tab name fd = [d|
  instance CFldDef $(liftType sch) $(liftType tab) $(liftType name) where
    type TFldDef $(liftType sch) $(liftType tab) $(liftType name) =
      $(liftType fd)
  |]

thTabDef :: Name -> NameNS -> TabDef -> DecsQ
thTabDef sch name td = [d|
  instance CTabDef $(liftType sch) $(liftType name) where
    type TTabDef $(liftType sch) $(liftType name) = $(liftType td)
  |]

thRelDef :: Name -> NameNS -> RelDef -> DecsQ
thRelDef sch name rd = [d|
  instance CRelDef $(liftType sch) $(liftType name) where
    type TRelDef $(liftType sch) $(liftType name) = $(liftType rd)
  |]

thSchema
  :: Name -- ^ schema name
  -> (Map NameNS TypDef
    , Map (NameNS,Text) FldDef
    , Map NameNS TabDef
    , Map NameNS RelDef)
  -> DecsQ
thSchema schName (mtyp, mfld, mtab, mrel) =
  L.concat <$> sequence
    [ sequence [dataD (pure []) schName [] Nothing [] []]
    , L.concat <$> traverse (uncurry $ thTypDef schName) (M.toList mtyp)
    , L.concat <$> traverse (\((a,b),c) -> thFldDef schName a b c) (M.toList mfld)
    , L.concat <$> traverse (uncurry $ thTabDef schName) (M.toList mtab)
    , L.concat <$> traverse (uncurry $ thRelDef schName) (M.toList mrel)
    , [d|
        instance CSchema $(schQ) where
          type TTabs $(schQ) = $(liftType $ keys mtab)
          type TRels $(schQ) = $(liftType $ keys mrel)
          type TTypes $(schQ) = $(liftType $ keys mtyp)
        |] ]
  where
    schQ = liftType schName

mkSchema :: ByteString -> String -> [Text] -> DecsQ
mkSchema connStr schName dbSchNames = do
  defs <- fmap getDefs $ runIO $ do
    conn <-
      catch (connectPostgreSQL connStr) (throwM . ConnectException connStr)
    getSchema conn dbSchNames
  thSchema (mkName schName) defs
