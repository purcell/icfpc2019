module ProblemDesc where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Map (Map)
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

data Point =
  Point
    { x :: !Int
    , y :: !Int
    }
  deriving (Eq, Ord, Show)

data Region =
  Region (NonEmpty Point)
  deriving (Show)

data Booster
  = ManipulatorExtension
  | FastWheels
  | Drill
  | MysteriousPoint
  deriving (Show)

data ProblemDesc =
  ProblemDesc
    { problemMap :: Region
    , problemStart :: Point
    , problemObstacles :: [Region]
    , boosterLocations :: Map Point Booster
    }
  deriving (Show)

parseProblem :: Parser ProblemDesc
parseProblem =
  ProblemDesc <$> parseRegion <*> (hash *> parsePoint) <*>
  (hash *> (parseRegion `sepBy` char ';')) <*>
  (hash *> (Map.fromList <$> parseBoosterLocs))
  where
    hash = char '#'
    parseRegion =
      Region <$> ((:|) <$> parsePoint <*> many (char ',' *> parsePoint))
    parsePoint =
      Point <$> (char '(' *> parseNat) <*> (char ',' *> parseNat <* char ')')
    parseNat = read <$> many1 digit
    parseBoosterLocs :: Parser [(Point, Booster)]
    parseBoosterLocs =
      ((\c p -> (p, c)) <$> parseBooster <*> parsePoint) `sepBy` char ';'
    parseBooster =
      (char 'B' *> pure ManipulatorExtension) <|> (char 'F' *> pure FastWheels) <|>
      (char 'L' *> pure Drill) <|>
      (char 'X' *> pure MysteriousPoint)
