module ASTSpec where

-- import AST
-- import TestPrelude

-- test_subtypeOf :: TestTree
-- test_subtypeOf =
--   testGroup
--     "subtypeOf"
--     [ -- min less than max but not vice versa because min allows strictly more
--       -- fields
--       recordTyMax' [] `notIsSubtypeOf` recordTyMin' [],
--       recordTyMin' [] `isSubtypeOf` recordTyMax' [],
--       recordTyMin' [("x", Universe 0)] `isSubtypeOf` recordTyMax' [("x", Universe 0)],
--       recordTyMax' [("x", Universe 0)] `notIsSubtypeOf` recordTyMin' [("x", Universe 0)],
--       -- for both kinds of records
--       recordTyMin' [("x", Universe 0)]
--         `isSubtypeOf` recordTyMin' [("x", Universe 0), ("y", Universe 0)],
--       recordTyMax' [("x", Universe 0), ("y", Universe 0)]
--         `isSubtypeOf` recordTyMax' [("x", Universe 0)]
--     ]
--   where
--     isSubtypeOf :: Term' -> Term' -> TestTree
--     isSubtypeOf x y =
--       testCase (show x <> " ≤ " <> show y) $
--         x `subtypeOf` y @? "wasn't a subtype as expected"
--     notIsSubtypeOf :: Term' -> Term' -> TestTree
--     notIsSubtypeOf x y =
--       testCase (show x <> " ≰ " <> show y) $
--         not (x `subtypeOf` y) @? "wasn't a strict supertype as expected"
