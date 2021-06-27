module Main where

import MyPrelude
import Polysemy.Readline
import Repl

main :: IO ()
main = runM . runReadline defaultSettings $ repl
