
module Main where

import Control.Monad.IO.Class
import Control.Wire
import UI.NCurses

import Logic
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
  (h, w) <- screenSize
  runNetwork world turn clockSession_ (horizPos w) (vertPos h)

runNetwork world turn session horizPos vertPos = do
  win <- defaultWindow
  updateWindow win $ drawWorld world
  render

  ev <- getEvent win Nothing

  if ev == (Just (EventCharacter 'q'))
     || ev == (Just (EventCharacter 'Q'))
  then
    return (world, turn)
  else
    do (s, session') <- stepSession session
       (x, horizPos') <- stepWire horizPos s (Right ev)
       (y, vertPos') <- stepWire vertPos s (Right ev)

       world' <- fmap reduceConsumables $ case (x, y) of
         ((Right x'), (Right y')) -> movePlayer x' y' world
         _ -> fail $ "Movement error"

       if noMoreConsums world' then
         return (world', turn)
       else
         runNetwork world' (turn + 1) session' horizPos' vertPos'
