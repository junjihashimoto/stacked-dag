module StackedDag.Graphviz where

import qualified Data.List as L
import qualified Data.Text.Lazy as T
import qualified Data.String as S
import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as GT
import StackedDag.Base

genEdgesFromDot :: T.Text -> (Labels,Edges)
genEdgesFromDot dot = (labels,edges)
  where
   dg :: G.DotGraph String
   dg = G.parseDotGraph dot
   edges = mkEdges $ map (\v -> (read (G.fromNode v),([read (G.toNode v)]))) $ G.graphEdges dg
   getl' :: G.Attribute -> Maybe String
   getl' attr = case attr of
     (GT.Label (GT.StrLabel name)) -> Just (T.unpack name)
     (GT.Label name) -> Just (show name)
     _ -> Nothing
   getl :: G.Attributes -> String
   getl attrs = maybe "" id $ foldl (\s i-> s `mappend`  getl' i) Nothing attrs
   labels = mkLabels $ map (\v -> (read (G.nodeID v),getl (G.nodeAttributes v))) $ G.graphNodes dg


genAsciiFromDot :: T.Text -> T.Text
genAsciiFromDot dot = S.fromString $ uncurry edgesToText $ genEdgesFromDot dot
