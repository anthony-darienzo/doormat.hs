module Main where

import qualified Brick as B
import Shell

ui :: B.Widget ()
ui = B.str "Hello, world!"

main :: IO ()
main = B.simpleMain ui
