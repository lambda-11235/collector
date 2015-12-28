
module Main where

import Control.Monad.IO.Class
import UI.NCurses

import World

main :: IO ()
main = do (world', turns) <- runCurses $ do setEcho False
                                            setCursorMode CursorInvisible
                                            world <- randomWorld
                                            run world 1

          let points = getPoints $ getPlayer world'
              score = (fromInteger points) / (fromInteger turns) :: Float

          putStrLn $ "Points/Turns (" ++ (show points) ++ "/" ++ (show turns)
            ++ ") = " ++ (show score)


run :: World -> Integer -> Curses (World, Integer)
run world turn = do
  win <- defaultWindow
  updateWindow win $ drawWorld world
  render

  ev <- getEvent win Nothing

  if ev == (Just (EventCharacter 'q')) then
    return (world, turn)
  else
    do world' <- fmap reduceConsumables $ case ev of
         (Just (EventCharacter 'a')) -> movePlayer (-1) 0 world
         (Just (EventCharacter 'd')) -> movePlayer 1 0 world
         (Just (EventCharacter 'w')) -> movePlayer 0 (-1) world
         (Just (EventCharacter 's')) -> movePlayer 0 1 world
         _ -> return world

       if noMoreConsums world' then
         return (world', turn)
       else
         run world' (turn + 1)
