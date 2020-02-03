module Test.AutoDict exposing (tests)

import AutoDict as Dict
import List
import Test exposing (Test,describe,test)
import Expect

animals : Dict.Dict String String String
animals =
    Dict.fromList identity [ ( "Tom", "cat" ), ( "Jerry", "mouse" ) ]


tests : Test
tests =
    let
        buildTests =
            describe "build Tests"
                [ 
                    test "empty" 
                        <| \() -> 
                            Expect.equal (Dict.fromList identity []) (Dict.empty identity)
                    , test "singleton" 
                        <| \() -> 
                        Expect.equal (Dict.fromList identity [ ( "k", "v" ) ]) (Dict.singleton identity "k" "v")
                    , test "insert" 
                        <| \() -> 
                        Expect.equal (Dict.fromList identity [ ( "k", "v" ) ]) (Dict.insert "k" "v" (Dict.empty identity))
                    , test "insert replace" 
                        <| \() -> 
                        Expect.equal (Dict.fromList identity [ ( "k", "vv" ) ]) (Dict.insert "k" "vv" (Dict.singleton identity "k" "v"))
                    , test "update" 
                        <| \() -> 
                        Expect.equal (Dict.fromList identity [ ( "k", "vv" ) ]) (Dict.update "k" (\_ -> Just "vv") (Dict.singleton identity "k" "v"))
                    , test "update Nothing" 
                        <| \() -> 
                        Expect.equal (Dict.empty identity) (Dict.update "k" (\_ -> Nothing) (Dict.singleton identity "k" "v"))
                    , test "remove" 
                        <| \() -> 
                        Expect.equal (Dict.empty identity) (Dict.remove "k" (Dict.singleton identity "k" "v"))
                    , test "remove not found" 
                        <| \() -> 
                        Expect.equal (Dict.singleton identity "k" "v") (Dict.remove "kk" (Dict.singleton identity "k" "v"))
                ]

        queryTests =
            describe "query Tests"
                [ test "member 1" 
                    <| \() -> Expect.equal True (Dict.member "Tom" animals)
                , test "member 2" 
                    <| \() -> Expect.equal False (Dict.member "Spike" animals)
                , test "get 1" 
                    <| \() -> Expect.equal (Just "cat") (Dict.get "Tom" animals)
                , test "get 2" 
                    <| \() -> Expect.equal Nothing (Dict.get "Spike" animals)
                , test "size of empty dictionary" 
                    <| \() -> Expect.equal 0 (Dict.size (Dict.empty identity))
                , test "size of example dictionary" 
                    <| \() -> Expect.equal 2 (Dict.size animals)
                ]

        combineTests =
            describe "combine Tests"
                [ test "union" 
                    <| \() -> Expect.equal animals 
                        (Dict.union 
                            (Dict.singleton identity "Jerry" "mouse") 
                            (Dict.singleton identity "Tom" "cat"))
                , test "union collison" 
                    <| \() -> Expect.equal 
                        (Dict.singleton identity "Tom" "cat") 
                        (Dict.union (Dict.singleton identity "Tom" "cat") 
                        (Dict.singleton identity "Tom" "mouse"))
                , test "intersect" 
                    <| \() -> Expect.equal (Dict.singleton identity "Tom" "cat") 
                        (Dict.intersect animals (Dict.singleton identity "Tom" "cat"))
                , test "diff" 
                    <| \() -> Expect.equal 
                        (Dict.singleton identity "Jerry" "mouse") 
                        (Dict.diff animals (Dict.singleton identity "Tom" "cat"))
                ]

        transformTests =
            describe "transform Tests"
                [ test "filter" 
                    <| \() -> Expect.equal 
                        (Dict.singleton identity "Tom" "cat") 
                        (Dict.filter (\k _ -> k == "Tom") animals)
                , test "partition" 
                    <| \() -> Expect.equal 
                        (Dict.singleton identity "Tom" "cat", Dict.singleton identity "Jerry" "mouse" ) 
                        (Dict.partition (\k _ -> k == "Tom") animals)
                ]

        mergeTests =
            let
                insertBoth key leftVal rightVal dict =
                    Dict.insert key (leftVal ++ rightVal) dict

                s1 =
                    Dict.empty identity |> Dict.insert "u1" [ 1 ]

                s2 =
                    Dict.empty identity |> Dict.insert "u2" [ 2 ]

                s23 =
                    Dict.empty identity |> Dict.insert "u2" [ 3 ]

                b1 =
                    List.map (\i -> ( i, [ i ] )) (List.range 1 10) |> Dict.fromList identity 

                b2 =
                    List.map (\i -> ( i, [ i ] )) (List.range 5 15) |> Dict.fromList identity 

                bExpected =
                    [ ( 1, [ 1 ] ), ( 2, [ 2 ] ), ( 3, [ 3 ] ), ( 4, [ 4 ] ), ( 5, [ 5, 5 ] ), ( 6, [ 6, 6 ] ), ( 7, [ 7, 7 ] ), ( 8, [ 8, 8 ] ), ( 9, [ 9, 9 ] ), ( 10, [ 10, 10 ] ), ( 11, [ 11 ] ), ( 12, [ 12 ] ), ( 13, [ 13 ] ), ( 14, [ 14 ] ), ( 15, [ 15 ] ) ]
                
                merge = Dict.merge compare Dict.insert insertBoth Dict.insert
            in
                describe "merge Tests"
                    [ test "merge empties" <|
                        \() ->Expect.equal (Dict.empty identity)
                            (merge (Dict.empty identity) (Dict.empty identity) (Dict.empty identity))
                    , test "merge singletons in order" <|
                        \() -> Expect.equal [ ( "u1", [ 1 ] ), ( "u2", [ 2 ] ) ]
                            (merge s1 s2  (Dict.empty identity) |> Dict.toList)
                    , test "merge singletons out of order" <|
                        \() ->
                            Expect.equal [ ( "u1", [ 1 ] ), ( "u2", [ 2 ] ) ]
                                (merge s2 s1  (Dict.empty identity) |> Dict.toList)
                    , test "merge with duplicate key" <|
                        \() ->
                            Expect.equal [ ( "u2", [ 2, 3 ] ) ]
                                (merge s2 s23  (Dict.empty identity) |> Dict.toList)
                    , test "partially overlapping" <|
                        \() ->
                            Expect.equal bExpected
                                (merge b1 b2  (Dict.empty identity) |> Dict.toList)
                    ]
        replaceMappingTest =
                describe "replaceMapping Test"
                    [ test "inverse sorting for negated key representation" <|
                        \() -> Expect.equal
                            (Dict.fromList identity [(1,1),(2,2),(3,3)] |> Dict.replaceMapping negate |> Dict.toList)
                            [(3,3),(2,2),(1,1)]
                    , describe "if rehashing leads to collisions the key with the highes value under current hashing wins"
                        [ test "some ordering in both hashinngs" <|
                            \() -> Expect.equal
                                (Dict.fromList identity [(-1,-1),(1,1)] |> Dict.replaceMapping abs |> Dict.toList)
                                [(1,1)]
                        , test "inver ordering in new hashing" <|
                            \() -> Expect.equal
                                (Dict.fromList identity [(-1,-1),(1,1)] |> Dict.replaceMapping (\n -> -(abs n)) |> Dict.toList)
                                [(1,1)]
                        ]
                    ]
        eqTest = describe "eqTest"
            [ test "some" <|
                \() -> 
                    let
                        list = [1,2,3] |> List.map (\n -> (n,n))
                        dict1 = Dict.fromList String.fromInt list
                        dict2 = Dict.fromList identity list
                    in
                        Expect.equal
                            (Dict.eq dict1 dict2)
                            True
            ]
    in
        describe "Dict Tests"
            [ buildTests
            , queryTests
            , combineTests
            , transformTests
            , mergeTests
            , replaceMappingTest
            , eqTest
            ]
