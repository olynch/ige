module IGE.Serialization where

import Protolude hiding (ByteString)
import Data.Aeson
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.ByteString.Lazy

graphToBS :: (ToJSON n, ToJSON e) => Gr n e -> ByteString
graphToBS g = encode (labNodes g, labEdges g)

graphFromBS :: (FromJSON n, FromJSON e) => ByteString -> Maybe (Gr n e)
graphFromBS bs = uncurry mkGraph <$> decode bs
