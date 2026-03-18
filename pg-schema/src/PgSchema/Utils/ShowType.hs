module PgSchema.Utils.ShowType where

import Data.List as L
import Data.String
import Data.Text as T
import PgSchema.Schema
import Prelude as P


class ShowType a where
  showType :: a -> Text

instance ShowType Text where
  showType = fromString . P.show

instance ShowType a => ShowType (Maybe a) where
  showType Nothing  = "'Nothing"
  showType (Just a) = "('Just " <> showType a <> ")"

instance ShowType Bool where
  showType True  = "'True"
  showType False = "'False"

instance ShowType a => ShowType [a] where
  showType = (\x -> "'[ " <> x <> " ]") . T.intercalate "," . L.map showType

instance (ShowType a, ShowType b) => ShowType (a,b) where
  showType (a,b) = "'( " <> showType a <> "," <> showType b <> " )"

instance ShowType NameNS where
  showType NameNS{..} =
    "( " <> showType nnsNamespace <> " ->> " <> showType nnsName <> " )"

instance ShowType TypDef where
  showType TypDef{..} = "'TypDef " <> T.intercalate " "
    [showType typCategory, showType typElem, showType typEnum]

instance ShowType FldDef where
  showType FldDef{..} = "'FldDef " <> T.intercalate " "
    [showType fdType, showType fdNullable, showType fdHasDefault]

instance ShowType TabDef where
  showType TabDef{..} = "'TabDef " <> T.intercalate " "
    [showType tdFlds, showType tdKey, showType tdUniq]

instance ShowType RelDef where
  showType RelDef{..} = "'RelDef " <> T.intercalate " "
    [showType rdFrom, showType rdTo, showType rdCols]

instance ShowType Ref where
  showType Ref{..} = "'Ref " <> T.intercalate " "
    [showType fromName, showType fromDef, showType toName, showType toDef]

instance ShowType (RecField NameNS) where
  showType = \case
    RFEmpty s     -> "'RFEmpty " <> showType s
    RFPlain fd    -> "'RFPlain " <> showType fd
    RFAggr fd fn b -> "'RFAggr " <> T.intercalate " " [showType fd, showType fn, showType b]
    RFToHere t rr -> "'RFToHere " <> showType t <> " " <> showType rr
    RFFromHere t rr -> "'RFFromHere " <> showType t <> " " <> showType rr
    RFSelfRef t rr -> "'RFSelfRef " <> showType t <> " " <> showType rr

instance ShowType AggrFun where
  showType = \case
    ACount -> "'ACount"
    AMin -> "'AMin"
    AMax -> "'AMax"
    ASum -> "'ASum"
    AAvg -> "'AAvg"
