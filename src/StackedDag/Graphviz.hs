{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}

module StackedDag.Graphviz where

import qualified Data.Text.Lazy as T
import qualified Data.String as S
import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as GT
import StackedDag.Base

genEdgesFromDot :: T.Text -> (Labels T.Text String,Edges T.Text)
genEdgesFromDot dot = (labels,edges)
  where
   dg :: G.DotGraph T.Text
   dg = G.parseDotGraph dot
   edges = mkEdges $ map (\v -> ( (G.fromNode v),([ (G.toNode v)]))) $ G.graphEdges dg
   getl' :: G.Attribute -> Maybe String
   getl' attr = case attr of
     (GT.Label (GT.StrLabel name)) -> Just (T.unpack name)
     (GT.Label name) -> Just (show name)
     _ -> Nothing
   getl :: G.Attributes -> String
   getl attrs = maybe "" id $ foldl (\s i-> s `mappend` getl' i) Nothing attrs
   labels = mkLabels $ map (\v -> ((G.nodeID v),getl (G.nodeAttributes v))) $ G.graphNodes dg


genAsciiFromDot :: T.Text -> T.Text
genAsciiFromDot dot = S.fromString $ uncurry edgesToText $ genEdgesFromDot dot
