{-# LANGUAGE ScopedTypeVariables #-}  -- Включаем некоторые расширения компилятора.
import Test.Tasty
import Test.Tasty.HUnit
import Data.Proxy
import Map
import qualified Data.Map.Strict as SMap
import MapInstance
import NaiveList(NaiveList)  -- Импортируем только тип NaiveList, но не его конструкторы Nil/Cons, чтобы не путались с конструкторами NaiveTree.
import NaiveTree

main :: IO ()
main = defaultMain testMap

{-|
  Генерирует группу тестов для конкретной реализации 'Map'
  с определённым именем.

  Мы хотим писать тесты один раз для всех возможных реализаций 'Map'.
  В чистом Haskell нам может помочь параметрический полиморфизм,
  но для этого нужно, чтобы в сигнатуре функции присутствовал
  тип из класса 'Map', который мы хотим протестировать.

  Специально для этих целей существует обёртка 'Data.Proxy', он
  позволяет передавать в функции даже типы высшего порядка.
-}
mapTests :: Map m => String -> Proxy m -> TestTree
mapTests name (_ :: Proxy m) =
    -- Чтобы можно было связать типовую переменную m здесь и в let ниже, нужно расширение ScopedTypeVariables.
    testGroup name [
        testGroup "toAscList, fromList" [
            testCase "toAscList . fromList sorts list" $
                let tr = fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                toAscList tr @?= [(1, "x"), (2, "a"), (3, "c")]
        ],
        testGroup "insert" [
            testCase "to empty map" $
                let map = insert 5 "x" (empty :: m Int String) in
                Map.lookup 5 map @?= Just "x"
            ,
            testCase "new key" $
                let map = insert 5 "x" (fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String) in
                Map.lookup 5 map @?= Just "x"
            ,
            testCase "existing key" $
                let map = insert 1 "x" (fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String) in
                Map.lookup 1 map @?= Just "x"
        ],
        testGroup "insertWith" [
            testCase "to empty map" $
                let map = insertWith (++) 5 "x" (empty :: m Int String) in
                Map.lookup 5 map @?= Just "x"
            ,
            testCase "new key" $
                let map = insertWith (++) 5 "x" (fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String) in
                Map.lookup 5 map @?= Just "x"
            ,
            testCase "existing key" $
                let map = insertWith (++) 1 "x" (fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String) in
                Map.lookup 1 map @?= Just "xb"
        ],
        testGroup "insertWithKey" [
            testCase "to empty map" $
                let f k new_v old_v = show k ++ new_v ++ old_v in
                let map = insertWithKey f 5 "x" (empty :: m Int String) in
                Map.lookup 5 map @?= Just "x"
            ,
            testCase "existing key" $
                let f k new_v old_v = show k ++ new_v ++ old_v in
                let map = insertWithKey f 1 "x" (fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String) in
                Map.lookup 1 map @?= Just "1xb"
        ],
        testGroup "delete" [
            testCase "non-existing key" $
                let map = delete 5 (fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String) in
                toAscList map @?= [(1, "b"), (2, "a"), (3, "c")]
            ,
            testCase "existing key" $
                let map = delete 1 (fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String) in
                toAscList map @?= [(2, "a"), (3, "c")]
        ],
        testGroup "adjust" [
            testCase "non-existing key" $
                let map = adjust ("new " ++) 5 (fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String) in
                Map.lookup 5 map @?= Nothing
            ,
            testCase "delete existing key" $
                let map = adjust ("new " ++) 1 (fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String) in
                Map.lookup 1 map @?= Just "new b"
        ],
        testGroup "adjustWithKey" [
            testCase "non-existing key" $
                let f k v = show k ++ v in
                let map = adjustWithKey f 5 (fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String) in
                Map.lookup 5 map @?= Nothing
                ,
            testCase "existing key" $
                let f k v = show k ++ v in
                let map = adjustWithKey f 1 (fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String) in
                Map.lookup 1 map @?= Just "1b"
        ],
        testGroup "update" [
            testCase "key was deleted by update" $
                let f x = if x == "a" then Just "new a" else Nothing in
                let map = update f 1 (fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String) in
                Map.lookup 1 map @?= Nothing
            ,
            testCase "key was updated by update" $
                let f x = if x == "a" then Just "new a" else Nothing in
                let map = update f 2 (fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String) in
                Map.lookup 2 map @?= Just "new a"
        ],
        testGroup "updateWithKey" [
            testCase "key was deleted by updateWihKey" $
                let f k x = if x == "a" then Just ((show k) ++ " new a") else Nothing in
                let map = updateWithKey f 1 (fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String) in
                Map.lookup 1 map @?= Nothing
            ,
            testCase "key was updated by updateWithKey" $
                let f k x = if x == "a" then Just ((show k) ++ " new a") else Nothing in
                let map = updateWithKey f 2 (fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String) in
                Map.lookup 2 map @?= Just "2 new a"
        ],
        testGroup "alter" [
            testCase "existing key" $
                let f _ = Just "c" in
                let map = alter f 1 (fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String) in
                toAscList map @?= [(1, "c"), (2, "a"), (3, "c")]
            ,
            testCase "non-existing key" $
                let f _ = Just "c" in
                let map = alter f 7 (fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String) in
                toAscList map @?= [(1, "b"), (2, "a"), (3, "c"), (7, "c")]
        ],
        testGroup "lookup" [
            testCase "existing key" $
                let map = fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String in
                Map.lookup 1 map @?= Just "b"
            ,
            testCase "non-existing key" $
                let map = fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String in
                Map.lookup 7 map @?= Nothing
        ],
        testGroup "member" [
            testCase "key is in map" $
                let map = fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String in
                member 1 map @?= True
            ,
            testCase "key isn't in map" $
                let map = fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String in
                member 5 map @?= False
        ],
        testGroup "notMember" [
            testCase "key is in map" $
                let map = fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String in
                notMember 1 map @?= False
            ,
            testCase "key isn't in map" $
                let map = fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String in
                notMember 5 map @?= True
        ],
        testGroup "null" [
            testCase "empty map" $
                let map = empty :: m Int String in
                Map.null map @?= True
            ,
            testCase "non empty map" $
                let map = fromList [(2, "a"), (1, "b"), (3, "c")] :: m Int String in
                Map.null map @?= False
        ],
        testGroup "size" [
            testCase "empty map" $
                let map = empty :: m Int String in
                size map @?= 0
            ,
            testCase "map with 3 different keys" $
                let map = fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                size map @?= 3
            ,
            testCase "singleton" $
                let map = singleton 2 "a" :: m Int String in
                size map @?= 1
        ]
    ]

testNaiveTree :: TestTree
testNaiveTree = testGroup "Test NaiveTree" [
        testGroup "merge" [
            testCase "merge empty" $
                merge Nil Nil @?= (Nil :: NaiveTree () ())
            ,
            testCase "merge two nodes" $
                -- Ваша реализация может выдавать другое дерево, соответствующее
                -- последовательности 1, 2.
                merge (Node 1 "a" Nil Nil) (Node 2 "b" Nil Nil)
                    @?= Node 1 "a" Nil (Node 2 "b" Nil Nil)
        ]
    ]

testMap :: TestTree
testMap = testGroup "Testing implementations of trees"
    [
        mapTests "Data.Map.Strict" (Proxy :: Proxy SMap.Map),
        mapTests "NaiveList" (Proxy :: Proxy NaiveList),
        mapTests "NaiveTree" (Proxy :: Proxy NaiveTree),
        testNaiveTree
    ]
