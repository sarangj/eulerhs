import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf

import Problems.Execute


main :: IO ()
main = defaultMain $ testGroup "Test All Problems"
  [ testProblem Problem1
  , testProblem Problem2
  , testProblem Problem3
  , testProblem Problem4
  , testProblem Problem5
  , testProblem Problem6
  , testProblem Problem7
  , testProblem Problem8
  , testProblem Problem9
  , testProblem Problem10
  ]

testProblem :: Problem -> TestTree
testProblem p = testCase (mkTestName p) $ 
  assertBool (mkIncorrectAnswerMessage expected answer) $ expected == answer
  where
    answer = execute p 
    expected = getExpected p 

mkTestName :: Problem -> TestName
mkTestName p = printf "%s Test" (show p) 

mkIncorrectAnswerMessage :: Int -> Int -> String
mkIncorrectAnswerMessage expected actual = printf
  "Incorrect Answer. Expected %d, got %d"
  expected
  actual

getExpected :: Problem -> Int
getExpected Problem1 = 233168
getExpected Problem2 = 4613732
getExpected Problem3 = 6857
getExpected Problem4 = 906609
getExpected Problem5 = 232792560
getExpected Problem6 = 25164150
getExpected Problem7 = 104743
getExpected Problem8 = 23514624000
getExpected Problem9 = 31875000
getExpected Problem10 = 142913828922
