import Test.Tasty
import Test.Tasty.HUnit

import Robots

main :: IO ()
main = defaultMain testsRobots

testsRobots :: TestTree
testsRobots = let
        walter = robot "Walter" 50 50
        weak = robot "Weak" 50 100
        strong = robot "Strong" 500 1000
        veryStrong = robot "VeryStrong" 800 1000
        dead = robot "Zombie" 100 (-100)
    in testGroup "Unit tests for Robots task"
        [ testCase "Test for getName" $
            getName walter @?= "Walter"

        , testCase "Test for getAttack" $
            getAttack weak @?= 50

        , testCase "Test for getHealth" $
            getHealth weak @?= 100

        , testCase "Test for setName" $
            setName "VeryStrong" weak @?= ("VeryStrong", 50, 100)

        , testCase "Test for setAttack" $
            setAttack 500 weak @?= ("Weak", 500, 100)

        , testCase "Test for setHealth" $
            setHealth 200 weak @?= ("Weak", 50, 200)

        , testCase "Test for printRobot" $
            printRobot walter @?= "Walter, attack: 50, health: 50"

        , testCase "Test for damage" $
            damage strong 500 @?= ("Strong", 500, 500)

        , testCase "Test for isAlive" $
            isAlive weak @?= True

        , testCase "Test for isAlive" $
            isAlive dead @?= False

        , testCase "Fight between alive robots" $
            fight weak strong @?= ("Strong", 500, 950)

        , testCase "Fight between alive and dead" $
            fight dead weak @?= weak

        , testCase "First round between strong and veryStrong" $
            threeRoundFight strong veryStrong @?= ("Strong", 500, 200)

        , testCase "Second round between strong and veryStrong" $
            threeRoundFight veryStrong strong @?= ("VeryStrong", 800, 500)

        , testCase "Test for survivers" $
            survivors @?= [("Bishop", 120, 800), ("David", 150, 1000)]
        ]
