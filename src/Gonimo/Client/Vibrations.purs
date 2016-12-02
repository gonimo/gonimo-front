module Gonimo.Client.Vibrations where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Foldable (sum)

foreign import data Vibrator :: *

foreign import _startVibration :: forall eff. Array Int -> Int -> Eff eff Vibrator

startVibration :: forall eff. Array Int -> Eff eff Vibrator
startVibration vals = do
  let delay = sum vals + 100
  _startVibration vals delay

foreign import stopVibration :: forall eff. Vibrator -> Eff eff Unit
