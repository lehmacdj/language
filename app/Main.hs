module Main where

import Control.Effect.Readline
import MyPrelude
import Repl

main :: IO ()
main = runM . runReadline defaultSettings $ repl
