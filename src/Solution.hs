module Solution where

import System.IO (IOMode(WriteMode), hClose, openFile)

data Action
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | NoOp
  | Rotate90Clockwise
  | Rotate90CounterClockwise
  | AttachManipulator Int Int
  | AttachFastWheels
  | AttachDrill

writeActions :: Int -> [Action] -> IO ()
writeActions solutionNum actions = do
  writeFile ("data/solutions/prob-" ++ show solutionNum ++ ".sol") $
    concatMap actionToStr actions

actionToStr :: Action -> String
actionToStr MoveUp = "W"
actionToStr MoveDown = "S"
actionToStr MoveLeft = "A"
actionToStr MoveRight = "D"
actionToStr NoOp = "Z"
actionToStr Rotate90Clockwise = "E"
actionToStr Rotate90CounterClockwise = "Q"
actionToStr (AttachManipulator x y) = "B(" ++ show x ++ "," ++ show y ++ ")"
actionToStr AttachFastWheels = "F"
actionToStr AttachDrill = "L"
