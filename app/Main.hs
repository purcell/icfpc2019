module Main where

import ProblemDesc
import Solver
import Solution

import System.Environment (getArgs)
import System.Exit (die)
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [problemFile, solutionFile] -> solveFromFile problemFile solutionFile
    _ -> die "Usage: thisprog problemFile solutionFile"

solveFromFile :: FilePath -> FilePath -> IO ()
solveFromFile problemFile solutionFile = do
  parseResult <- parseFromFile parseProblem problemFile
  case parseResult of
    Left err  -> die $ "Failed to parse input: " ++ problemFile
    Right problemDesc  -> writeFile solutionFile $ solutionString $ solve problemDesc
