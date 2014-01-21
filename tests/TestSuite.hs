import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Mower.Core

main :: IO ()
main = defaultMainWithOpts
       [ testCase "should_turn_mower_to_the_east" should_turn_mower_to_the_east
       , testProperty "listRevRevId" propListRevRevId
       ] mempty

should_turn_mower_to_the_east :: Assertion
should_turn_mower_to_the_east = do
	let m = makeMower 0 0 North
	(turnRight m) @?= makeMower 0 0 East

propListRevRevId :: [Int] -> Property
propListRevRevId xs = not (null xs) ==> reverse (reverse xs) == xs