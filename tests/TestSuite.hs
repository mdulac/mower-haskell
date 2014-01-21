module Main
    ( main
    ) where

import Test.Framework (defaultMain)

import qualified Mower.Core.Tests

main :: IO ()
main = defaultMain
    [ Mower.Core.Tests.tests
    ]