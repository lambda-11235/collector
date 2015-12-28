
module Logic where

import Prelude hiding ((.), id)
import Control.Wire
import FRP.Netwire
import qualified UI.NCurses as NC

accumB :: (b -> a -> b) -> b -> Wire s e m a b
accumB f = loop
  where
    loop x = mkSFN $ \y -> let x' = f x y in (x', loop x')


horizSpeed :: Wire s () NC.Curses (Maybe NC.Event) Float
horizSpeed = mkSF_ $ \ev -> case ev of
  (Just (NC.EventCharacter 'a')) -> (-1)
  (Just (NC.EventCharacter 'A')) -> (-1)
  (Just (NC.EventCharacter 'd')) -> 1
  (Just (NC.EventCharacter 'D')) -> 1
  _ -> 0

horizPos :: (HasTime t s) => Integer -> Wire s () NC.Curses (Maybe NC.Event) Integer
horizPos width = fmap ((`mod` width) . floor) $ accumB (+) 0 . horizSpeed


vertSpeed :: Wire s () NC.Curses (Maybe NC.Event) Float
vertSpeed = mkSF_ $ \ev -> case ev of
  (Just (NC.EventCharacter 'w')) -> (-1)
  (Just (NC.EventCharacter 'W')) -> (-1)
  (Just (NC.EventCharacter 's')) -> 1
  (Just (NC.EventCharacter 'S')) -> 1
  _ -> 0

vertPos :: (HasTime t s) => Integer -> Wire s () NC.Curses (Maybe NC.Event) Integer
vertPos height = fmap ((`mod` height) . floor) $ accumB (+) 0 . vertSpeed
