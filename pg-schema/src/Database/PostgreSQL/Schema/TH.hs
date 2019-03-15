module Database.PostgreSQL.Schema.TH where

import Control.Monad
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
import Database.Schema.TH
import GHC.Generics
import Language.Haskell.TH


class LiftType a where
  liftType :: a -> TypeQ

instance LiftType Text where
  liftType = pure . txtToSym

instance LiftType Name where
  liftType = conT

instance LiftType a => LiftType (Maybe a) where
  liftType = traverse liftType >=> maybeQ
    where
      maybeQ Nothing  = [t|'Nothing|]
      maybeQ (Just t) = appT [t|'Just|] (pure t)

instance LiftType Bool where
  liftType True  = [t|'True|]
  liftType False = [t|'False|]

instance LiftType a => LiftType [a] where
  liftType = fmap toPromotedList . traverse liftType

instance (LiftType a, LiftType b) => LiftType (a,b) where
  liftType (a,b) = [t| '( $(liftType a), $(liftType b) ) |]

instance LiftType TypDef where
  liftType TypDef{..} = [t| 'TypDef
    $(liftType typCategory) $(liftType typElem) $(liftType typEnum) |]

instance LiftType FldDef where
  liftType FldDef{..} = [t| 'FldDef
    $(liftType fdType) $(liftType fdNullable) $(liftType fdHasDefault) |]

instance LiftType TabDef where
  liftType TabDef{..} = [t| 'TabDef
    $(liftType tdFlds) $(liftType tdKey) $(liftType tdUniq) |]

instance LiftType RelDef where
  liftType RelDef{..} = [t| 'RelDef
    $(liftType rdFrom) $(liftType rdTo) $(liftType rdCols) |]

thTypDef :: Name -> Text -> TypDef -> DecsQ
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
      = flip normalC [] . mkName . T.unpack . ((toTitle name <> "_") <>)
      <$> typEnum

thFldDef :: Name -> Text -> Text -> FldDef -> DecsQ
thFldDef sch tab name fd = [d|
  instance CFldDef $(liftType sch) $(liftType tab) $(liftType name) where
    type TFldDef $(liftType sch) $(liftType tab) $(liftType name) =
      $(liftType fd)
  |]

thTabDef :: Name -> Text -> TabDef -> DecsQ
thTabDef sch name td = [d|
  instance CTabDef $(liftType sch) $(liftType name) where
    type TTabDef $(liftType sch) $(liftType name) = $(liftType td)
  |]

thRelDef :: Name -> Text -> RelDef -> DecsQ
thRelDef sch name rd = [d|
  instance CRelDef $(liftType sch) $(liftType name) where
    type TRelDef $(liftType sch) $(liftType name) = $(liftType rd)
  |]

thSchema
  :: Name -- ^ schema name
  -> Text -- ^ database schema name
  -> (Map Text TypDef
    , Map (Text,Text) FldDef
    , Map Text TabDef
    , Map Text RelDef)
  -> DecsQ
thSchema schName dbSchName (mtyp, mfld, mtab, mrel) =
  L.concat <$> sequence
    [ sequence [dataD (pure []) schName [] Nothing [] []]
    , L.concat <$> traverse (uncurry $ thTypDef schName) (M.toList mtyp)
    , L.concat <$> traverse (\((a,b),c) -> thFldDef schName a b c) (M.toList mfld)
    , L.concat <$> traverse (uncurry $ thTabDef schName) (M.toList mtab)
    , L.concat <$> traverse (uncurry $ thRelDef schName) (M.toList mrel)
    , [d|
        instance CSchema $(schQ) where
          type TSchema $(schQ) = $(liftType dbSchName)
          type TTabs $(schQ) = $(liftType $ keys mtab)
          type TRels $(schQ) = $(liftType $ keys mrel)
          type TTypes $(schQ) = $(liftType $ keys mtyp)
        |] ]
  where
    schQ = liftType schName

mkSchema :: ByteString -> String -> Text -> DecsQ
mkSchema connStr schName dbSchName = do
  defs <- fmap getDefs $ runIO $ do
    conn <-
      catch (connectPostgreSQL connStr) (throwM . ConnectException connStr)
    getSchema conn dbSchName
  thSchema (mkName schName) dbSchName defs
