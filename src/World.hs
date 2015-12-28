
module World where

import Control.Monad (replicateM)
import Control.Monad.IO.Class
import qualified Data.Set as S
import System.Random
import UI.NCurses


type Consumable = (Integer, Integer)

data Player = Player { getXPos :: Integer
                     , getYPos :: Integer
                     , getPoints :: Integer }
            deriving (Eq, Show)

data World = World { getPlayer :: Player
                   , getConsums :: (S.Set Consumable)}
           deriving (Eq, Show)


playerSymbol = "@"
consumSymbol = "*"
pointsPerConsumable = 100
maxConsums = 5


noMoreConsums :: World -> Bool
noMoreConsums (World _ consums) = consums == S.empty


movePlayer :: Integer -> Integer -> World -> Curses World
movePlayer x y (World (Player _ _ points) consums) =
  return (World (Player x y points) consums)

reduceConsumables :: World -> World
reduceConsumables world@(World (Player x y points) consums) =
  if S.member (x, y) consums then
    World (Player x y (points + pointsPerConsumable)) (S.delete (x, y) consums)
  else
    world


drawWorld :: World -> Update ()
drawWorld (World player consums) = do clear
                                      drawPlayer player
                                      drawConsums $ S.toList consums
  where
    drawPlayer (Player x y _) = do moveCursor y x
                                   drawString playerSymbol
    drawConsums [] = return ()
    drawConsums ((x, y):cs) = do moveCursor y x
                                 drawString consumSymbol
                                 drawConsums cs

initPlayer = Player 0 0 0

randomConsum :: Curses Consumable
randomConsum = do (h, w) <- screenSize
                  x <- liftIO $ randomRIO (0, (w-1))
                  y <- liftIO $ randomRIO (0, (h-1))
                  return $ (x, y)

randomWorld :: Curses World
randomWorld = do consums <- replicateM maxConsums randomConsum
                 return $ World initPlayer (S.fromList consums)
