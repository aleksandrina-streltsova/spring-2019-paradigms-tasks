import Test.Tasty
import Test.Tasty.HUnit

import Basics

main :: IO ()
main = defaultMain testsBasics

testsBasics :: TestTree
testsBasics = testGroup "Unit tests for Basics tasks"
    [testCase "head' works on non-empty list" $
        head' [1,2,3] @?= 1

    , testCase "head' works on infinite list" $
        head' [1..] @?= 1

    , testCase "tail' works on non-empty list too" $
        tail' [1,2,3] @?= [2,3]

    , testCase "tail' works on infinite list too" $
        take' 100 (tail' [1..]) @?= take' 100 [2..]

    , testCase "tail' works on empty list" $
        tail' ([] :: [Int]) @?= []

    , testCase "take' takes 1 element from 3-element list" $
        take' 1 [1,2,3] @?= [1]

    , testCase "take' takes 3 elements from infinite list" $
        take' 3 [2..] @?= [2,3,4]

    , testCase "take' works on empty lists as expected" $
        take' 3 ([] :: [Int]) @?= []

    , testCase "drop' drops 1 element from 3-element list" $
        drop' 1 [1,2,3] @?= [2,3]

    , testCase "drop' drops 3 element from infinite list" $
        take' 100 (drop' 3 [1..]) @?= take' 100 [4..]

    , testCase "filter' selects only even numbers from 0 to 10" $
        filter' even [0..10] @?= [0,2..10]

    , testCase "filter' selects only even numbers from infinite list" $
        take 100 (filter' even [0..]) @?= take 100 [0,2..]

    , testCase "foldl'' can be used for finding sum of elements" $
        foldl'' (+) 0 [1,2,3] @?= 6

    , testCase "concat' works on finite lists as expected" $
        concat' [1,2,3] [4,5,6] @?= [1..6]

    , testCase "concat' works on infinite list and finite list as expected" $
        take' 100 (concat' [1,2,3] [4..]) @?= take' 100 [1..]

    , testCase "quickSort actualy sorts the list" $
        quickSort' [5,2,3,4,1] @?= [1..5]
    ]
