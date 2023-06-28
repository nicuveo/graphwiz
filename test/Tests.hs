{- AUTOCOLLECT.MAIN

group_type = tree
custom_main = True

-}

import "this" Prelude

import Test.Tasty

{- AUTOCOLLECT.MAIN.imports -}

main :: IO ()
main = defaultMain $ testGroup "graphwiz" {- AUTOCOLLECT.MAIN.tests -}
