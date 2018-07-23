module Main where

import Lib (app)

import qualified Network.Wai.Handler.Warp as Wai

main :: IO ()
main = Wai.run 3003 (Lib.app "hi")
