import Lib

import Data.Either (isRight)
import Test.HUnit

testFile :: FilePath -> Test
testFile f = TestLabel f . TestCase . assert . fmap assertFailure . readZone $ f
  where assertFailure (Left m) = assertString m
        assertFailure (Right _) = assert True

tests = TestList [ testFile "test/simple.zone"
                 ]

main :: IO ()
main = runTestTT tests >>= print
