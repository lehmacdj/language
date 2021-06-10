module TestPrelude
  ( module MyPrelude,
    module Test.Tasty,
    module Test.Tasty.HUnit,
    module Test.Tasty.QuickCheck,
  )
where

import MyPrelude hiding (assert)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
