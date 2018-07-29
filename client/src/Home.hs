-- | Haskell language pragma
{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}
-- | Haskell module declaration
module Main where

-- | Miso framework import
import           Data.Monoid   ((<>))
import           Miso
import           Miso.String
-- | Type synonym for an application model
import           Common.Action
import           Common.Model
import           Common.View
import           Control.Lens  hiding (view)

-- | Sum type for application events
main :: IO ()
main =
  Miso.miso $ \currentURI -> App
    { initialAction = SayHelloWorld
    , model         = initialModel
    , update        = fromTransition . updateModel
    , view          = viewModel
    , events        = defaultEvents
    , subs          = []
    , mountPoint    = Nothing
    }

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Transition Action Model ()
updateModel AddOne = val += 1
updateModel SubtractOne = val -= 1
updateModel NoOp = pure ()
updateModel SayHelloWorld =
  scheduleIO $ do
    putStrLn "Hello Dung"
    pure NoOp

