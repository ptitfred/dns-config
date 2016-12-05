import Lib

import Data.Either (isRight)
import Data.List (group, sort)
import Test.HUnit

testParsing :: FilePath -> Test
testParsing f = TestLabel f . TestCase . assert . fmap assertFailure . readZone $ f
  where assertFailure (Left m) = assertString m
        assertFailure (Right _) = assert True

testCleanup :: FilePath -> Test
testCleanup f = TestLabel ("cleanup " ++ f) . TestCase . assert . fmap assertSimplification . readZone $ f
  where assertSimplification (Right (Zone _ records)) = assert . noDuplicate . recordNames $ records
        assertSimplification (Left e) = assertString e
        noDuplicate = all (==1) . map length . group . sort
        recordNames = map recordName . filter isExplicit
        isExplicit Explicit {} = True
        isExplicit _           = False
        recordName (Explicit n _ _ _ _) = n
        recordName _ = ""

tests = TestList [ testParsing "test/simple.zone"
                 , testCleanup "test/simple.zone"
                 ]

main = runTestTT tests >>= print
