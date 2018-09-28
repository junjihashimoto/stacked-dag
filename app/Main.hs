module Main where

import StackedDag
import Options.Applicative
import qualified Data.Text.Lazy.IO as T

data Command =
    Dot FilePath
  | Samples

fromdot :: Parser Command
fromdot = Dot
         <$> (argument str (metavar "DotFile"))

samples :: Parser Command
samples = pure Samples

parse :: Parser Command
parse = subparser $
        command "dot" (info fromdot (progDesc "convert graphviz-dot-file to ascii-dag")) <>
        command "samples" (info samples (progDesc "show samples"))

runCmd :: Command -> IO ()
runCmd (Dot ifile) = do
  v <- T.readFile ifile
  T.putStr $ genAsciiFromDot v


runCmd (Samples) = do
  putStr $ edgesToText samplelabels sampledat
  putStrLn "---"
  putStr $ edgesToText ( mkLabels [
                           (0,"l0"),
                           (1,"l1"),
                           (2,"l2"),
                           (3,"l3")
                    ]) ( mkEdges [
                           (0,[3]),
                           (1,[2])
                           ])
  putStrLn "---"
  putStr $ edgesToText ( mkLabels [
                           (0,"l0"),
                           (1,"l1"),
                           (2,"l2"),
                           (3,"l3")
                    ]) ( mkEdges [
                           (0,[1,2,3])
                           ])
  putStrLn "---"
  putStr $ edgesToText ( mkLabels [
                           (0,"l0"),
                           (1,"l1"),
                           (2,"l2"),
                           (3,"l3"),
                           (4,"l4")
                    ]) ( mkEdges [
                           (0,[4]),
                           (1,[4]),
                           (2,[4]),
                           (3,[4])
                           ])
  putStrLn "---"
  putStr $ edgesToText ( mkLabels []) ( mkEdges [
                                          (0,[1,2]),
                                          (1,[2])
                                          ])
  putStrLn "---"
  putStr $ edgesToText ( mkLabels []) ( mkEdges [
                                          (0,[1,3]),
                                          (1,[2]),
                                          (2,[3])
                                          ])

sampledat :: Edges
sampledat = mkEdges [
  (0,[2]),
  (1,[2]),
  (2,[3]),
  (4,[3]),
  (6,[3]),
  (3,[5])
  ]

samplelabels :: Labels
samplelabels = mkLabels [
  (0,"l0"),
  (1,"l1"),
  (2,"l2"),
  (3,"l3"),
  (5,"l5"),
  (4,"l4"),
  (6,"l6")
  ]

opts :: ParserInfo Command
opts = info (parse <**> helper) idm

main :: IO ()
main = execParser opts >>= runCmd
