{-# LANGUAGE RecordWildCards #-}

module Solver where

import BFS (bfs)
import Control.Monad (when)
import qualified Data.Foldable as F
import Data.Function (on)
import Data.List (groupBy)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import ProblemDesc
import Solution
import State

solve :: ProblemDesc -> [Action]
solve prob =
  actions $ fillUntilDone $ initialState prob firstTarget' unfilledPoints
  where
    (firstTarget, unfilledPoints) = Set.splitAt 0 $ pointsToFill prob
    firstTarget' = Set.elemAt 0 firstTarget

fillUntilDone :: State -> State
fillUntilDone state =
  let (newCurrentPoint, actionTaken) =
        takeAction (currentPoint state) (targetPoint state)
      newTargetPoint =
        if currentPoint state == targetPoint state
          then Set.elemAt 0 $ unfilledPoints state
          else targetPoint state
      newUnfilledPoints = Set.delete newCurrentPoint $ unfilledPoints state
      newState =
        State
          { currentPoint = newCurrentPoint
          , targetPoint = newTargetPoint
          , unfilledPoints = newUnfilledPoints
          , actions = actions state ++ [actionTaken]
          }
   in if Set.null $ unfilledPoints newState
        then newState
        else fillUntilDone newState

takeAction :: Point -> Point -> (Point, Action)
takeAction current target = (current, MoveUp)

reachableNeighbours :: ProblemDesc -> Point -> [Point]
reachableNeighbours prob p = filter (reachable prob p) (neighbours p)

reachable :: ProblemDesc -> Point -> Point -> Bool
reachable ProblemDesc {..} p p' = not (any (rightOfEdge p') curEdges)
  where
    allEdges = edges problemMap ++ concatMap edges problemObstacles
    curEdges = filter (onEdge p) allEdges

pointsToFill :: ProblemDesc -> Set Point
pointsToFill prob =
  Set.fromList $ bfs (reachableNeighbours prob) (problemStart prob)

problemEdges :: ProblemDesc -> [(Point, Point)]
problemEdges ProblemDesc {..} =
  edges problemMap ++ concatMap edges problemObstacles

rightOfEdge :: Point -> (Point, Point) -> Bool
rightOfEdge (Point x y) (Point x1 y1, Point x2 y2)
  | x1 < x2 = y < y1
  | y1 < y2 = x > x1
  | x1 > x2 = y > y1
  | y1 > y2 = x < x1
  | otherwise = False

onEdge :: Point -> (Point, Point) -> Bool
onEdge (Point x y) (Point x1 y1, Point x2 y2)
  | x1 == x2 && x == x1 = y >= min y1 y2 && y <= max y1 y2
  | y1 == y2 && y == y1 = x >= min x1 x2 && x <= max x1 x2
  | otherwise = False

neighbours :: Point -> [Point]
neighbours (Point x y) =
  [Point (x - 1) y, Point (x + 1) y, Point x (y - 1), Point x (y + 1)]

edges :: Region -> [(Point, Point)]
edges (Region points) = zip points (tail (cycle points))

showPoints :: Set Point -> String
showPoints points = concatMap line [ymax,ymax - 1 .. ymin]
  where
    line y = map (\x -> square x y) [xmin .. xmax] ++ "\n"
    square x y =
      if Point x y `Set.member` points
        then '#'
        else ' '
    xmin = F.minimum (x <$> Set.toList points)
    xmax = F.maximum (x <$> Set.toList points)
    ymin = F.minimum (y <$> Set.toList points)
    ymax = F.maximum (y <$> Set.toList points)

regionEdgePoints :: Region -> Set Point
regionEdgePoints (Region ps) = Set.unions (go <$> (zip ps (tail (cycle ps))))
  where
    go (Point x1 y1, Point x2 y2) =
      Set.fromList
        [ Point x' y'
        | x' <- [(min x1 x2) .. (max x1 x2)]
        , y' <- [(min y1 y2) .. (max y1 y2)]
        ]
