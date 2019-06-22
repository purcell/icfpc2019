{-# LANGUAGE RecordWildCards #-}

module State where

import Data.Set
import ProblemDesc
import Solution

data State =
  State
    { currentPoint :: Point
    , targetPoint :: Point
    , unfilledPoints :: Set Point
    , actions :: [Action]
    }

initialState :: ProblemDesc -> Point -> Set Point -> State
initialState ProblemDesc {..} initialTarget unfilled =
  State
    { currentPoint = problemStart
    , targetPoint = initialTarget
    , unfilledPoints = unfilled
    , actions = []
    }
