{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Solver where

import Control.Monad (when)
import qualified Control.Monad.State.Strict as ST
import qualified Data.Foldable as F
import Data.Function (on)
import Data.List (groupBy)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import ProblemDesc
import Solution

solve :: ProblemDesc -> [Action]
solve prob = [MoveUp, MoveDown, Rotate90Clockwise]

regionPoints :: Region -> Set Point
regionPoints region@(Region points) =
  regionEdgePoints region -- `Set.union` Set.unions (line <$> [ymin .. ymax])
  where
    line :: Int -> Set Point
    line y = go False Set.empty [xmin - 1 .. xmax]
      where
        go _ contained [] = contained
        go inside contained (x:xs) =
          let p = Point x y
              inside' = inside /= elem x (verticalEdges y)
           in go
                inside'
                (if inside'
                   then p `Set.insert` contained
                   else contained)
                xs
    verticalEdges y =
      [ v1x
      | (Point v1x v1y, Point v2x v2y) <- edges region
      , v1x == v2x
      , min v1y v2y <= y
      , max v1y v2y >= y
      ]
    xmin = F.minimum ((\(Point x _) -> x) <$> points)
    xmax = F.maximum ((\(Point x _) -> x) <$> points)
    ymin = F.minimum ((\(Point _ y) -> y) <$> points)
    ymax = F.maximum ((\(Point _ y) -> y) <$> points)

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
{-
xpxxxxxx
x  p   xxx
x        x
x    ooo x
x    o o x
x    ooo x
x        x
xxxooxxxxx

toFill = (pointsAlongEdge - coincidingPointsOnObstacleEdges) + interiorArea

-}
