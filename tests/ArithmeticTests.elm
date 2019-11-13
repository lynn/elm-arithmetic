module ArithmeticTests exposing (suite)

import Arithmetic
    exposing
        ( chineseRemainder
        , cubeRoot
        , divides
        , divisorCount
        , divisors
        , exactIntCubeRoot
        , exactIntSquareRoot
        , extendedGcd
        , fromBase
        , gcd
        , intCubeRoot
        , intSquareRoot
        , isCoprimeTo
        , isCube
        , isEven
        , isOdd
        , isPrime
        , isSquare
        , lcm
        , modularInverse
        , powerMod
        , primesBelow
        , properDivisors
        , safeSquareRoot
        , toBase
        , totient
        )
import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (float, int)
import Test exposing (Test, describe, fuzz, fuzz2, test)


suite : Test
suite =
    describe "Arithmetic module"
        [ describe "Arithmetic.isEven"
            [ test "isEven Detects even numbers correctly" <|
                \_ ->
                    List.all isEven [ 2, 4, 6, 8, 10, 12, 14, 16, 18, 20 ]
                        |> Expect.true "Expected all to be even"
            , test "isEven Detects odd numbers correctly" <|
                \_ ->
                    List.any isEven [ 1, 3, 5, 7, 9, 11, 13, 15, 17, 19 ]
                        |> Expect.false "Expected all to be odd"
            ]
        , describe "Arithmetic.isOdd"
            [ test "isOdd Detects odd numbers correctly" <|
                \_ ->
                    List.all isOdd [ 1, 3, 5, 7, 9, 11, 13, 15, 17, 19 ]
                        |> Expect.true "Expected all to be odd"
            , test "isOdd Detects even numbers correctly" <|
                \_ ->
                    List.any isOdd [ 2, 4, 6, 8, 10, 12, 14, 16, 18, 20 ]
                        |> Expect.false "Expected all to be even"
            ]
        , describe "Arithmetic.toBase"
            [ test "toBase converts to base 2 correctly" <|
                \_ ->
                    toBase 2 42
                        |> Expect.equal [ 1, 0, 1, 0, 1, 0 ]
            , test "toBase converts base 8 numbers correctly" <|
                \_ ->
                    toBase 8 6694
                        |> Expect.equal [ 1, 5, 0, 4, 6 ]
            , fuzz int "toBase does nothing on base 10 numbers" <|
                \fuzzInt ->
                    toBase 10 fuzzInt
                        |> Expect.equal (getDigits fuzzInt)
            ]
        , describe "Arithmetic.fromBase"
            [ test "fromBase converts from base 2 correctly" <|
                \_ ->
                    fromBase 2 [ 1, 0, 1, 0, 1, 0 ]
                        |> Expect.equal 42
            , fuzz int "fromBase does nothing on base 10 numbers" <|
                \fuzzInt ->
                    fromBase 10 (getDigits fuzzInt)
                        |> Expect.equal (abs fuzzInt)
            ]
        , describe "Arithmetic.safeSquareRoot"
            [ fuzz float "safeSquareRoot returns Nothing for negatives and Just num for positives" <|
                \fuzzFloat ->
                    if fuzzFloat < 0 then
                        safeSquareRoot fuzzFloat
                            |> Expect.equal Nothing

                    else
                        safeSquareRoot fuzzFloat
                            |> Expect.equal (Just (sqrt fuzzFloat))
            ]
        , describe "Arithmetic.intSquareRoot"
            [ test "intSquareRoot returns the correct value for 21" <|
                \_ ->
                    intSquareRoot 21
                        |> Expect.equal 5
            , test "intSquareRoot returns the correct value for 97" <|
                \_ ->
                    intSquareRoot 97
                        |> Expect.equal 10
            ]
        , describe "Arithmetic.exactIntSquareRoot"
            [ test "exactIntSquareRoot returns Nothing for 35" <|
                \_ ->
                    exactIntSquareRoot 35
                        |> Expect.equal Nothing
            , test "exactIntSquareRoot returns Just 6 for 36" <|
                \_ ->
                    exactIntSquareRoot 36
                        |> Expect.equal (Just 6)
            , test "exactIntSquareRoot returns Nothing for 37" <|
                \_ ->
                    exactIntSquareRoot 37
                        |> Expect.equal Nothing
            ]
        , describe "Arithmetic.isSquare"
            [ test "isSquare filters the squares out" <|
                \_ ->
                    List.filter isSquare (List.range 1 100)
                        |> Expect.equalLists [ 1, 4, 9, 16, 25, 36, 49, 64, 81, 100 ]
            ]
        , describe "Arithmetic.cubeRoot"
            [ fuzz float "cubeRoot returns correct values when fuzzily tested" <|
                \fuzzFloat ->
                    (cubeRoot <| abs fuzzFloat)
                        |> Expect.within (Absolute 0.000001) (abs fuzzFloat ^ (1 / 3))
            ]
        , describe "Arithmetic.intCubeRoot"
            [ test "intCubeRoot returns NaN for negatives" <|
                \_ ->
                    (isNaN <| toFloat <| intCubeRoot -5)
                        |> Expect.true "Expected intCubeRoot to return NaN"
            , test "intCubeRoot returns 0 for 0" <|
                \_ ->
                    intCubeRoot 0
                        |> Expect.equal 0
            , test "intCubeRoot returns the correct value" <|
                \_ ->
                    intCubeRoot 123
                        |> Expect.equal 5
            ]
        , describe "Arithmetic.exactIntCubeRoot"
            [ test "exactIntCubeRoot returns the exact cubes after mapping" <|
                \_ ->
                    List.map (\x -> x ^ 3) (List.filterMap exactIntCubeRoot (List.range 1 343))
                        |> Expect.equalLists [ 1, 8, 27, 64, 125, 216, 343 ]
            ]
        , describe "Arithmetic.isCube"
            [ test "isCube detects cubes properly" <|
                \_ ->
                    let
                        input =
                            125
                    in
                    isCube input
                        |> Expect.true ("Expected " ++ String.fromInt input ++ " to be a cube")
            ]
        , describe "Arithmetic.divides"
            [ test "divides returns true for correct input" <|
                \_ ->
                    let
                        num1 =
                            40

                        num2 =
                            8
                    in
                    divides num2 num1
                        |> Expect.true ("Expected " ++ String.fromInt num2 ++ " to divide " ++ String.fromInt num1)
            ]
        , describe "Arithmetic.divisors"
            [ let
                input =
                    20
              in
              test ("divisors returns the correct divisors for " ++ String.fromInt input) <|
                \_ ->
                    divisors input
                        |> Expect.equalLists [ 1, 2, 4, 5, 10, 20 ]
            , let
                input =
                    0
              in
              test ("divisors returns the correct divisors for " ++ String.fromInt input) <|
                \_ ->
                    divisors input
                        |> Expect.equalLists [ 1 ]
            ]
        , describe "Arithmetic.properDivisors"
            [ let
                input =
                    20
              in
              test ("properDivisors returns the correct divisors for " ++ String.fromInt input) <|
                \_ ->
                    properDivisors input
                        |> Expect.equalLists [ 1, 2, 4, 5, 10 ]
            , let
                input =
                    0
              in
              test ("properDivisors returns the correct divisors for " ++ String.fromInt input) <|
                \_ ->
                    properDivisors input
                        |> Expect.equalLists [ 1 ]
            ]
        , describe "Arithmetic.divisorCount"
            [ fuzz int "divisorCount correct for random input" <|
                \fuzzInt ->
                    divisorCount fuzzInt
                        |> Expect.equal (List.length (divisors fuzzInt))
            ]
        , describe "Arithmetic.gcd"
            [ test "gcd should return 2" <|
                \_ ->
                    gcd 2 0
                        |> Expect.equal 2
            , test "gcd should return 4" <|
                \_ ->
                    gcd 4 0
                        |> Expect.equal 4
            , test "gcd should return 10" <|
                \_ ->
                    gcd 10 100
                        |> Expect.equal 10
            , test "gcd should return 8" <|
                \_ ->
                    gcd 56 80
                        |> Expect.equal 8
            ]
        , describe "Arithmetic.lcm"
            [ test "lcm should return 0" <|
                \_ ->
                    lcm 0 3
                        |> Expect.equal 0
            , test "lcm, again, should return 0" <|
                \_ ->
                    lcm 3 0
                        |> Expect.equal 0
            , test "lcm should return 560" <|
                \_ ->
                    lcm 56 80
                        |> Expect.equal 560
            ]
        , describe "Arithmetic.isCoprimeTo"
            [ test "isCoprimeTo" <|
                \_ ->
                    isCoprimeTo 56 80
                        |> Expect.false "Should not be coprime"
            ]
        , describe "Arithmetic.totient"
            [ let
                expected =
                    60
              in
              test ("totient should return " ++ String.fromInt expected) <|
                \_ ->
                    totient 99
                        |> Expect.equal expected
            , let
                expected =
                    560
              in
              test ("totient should return " ++ String.fromInt expected) <|
                \_ ->
                    totient 1450
                        |> Expect.equal expected
            ]
        , describe "Arithmetic.extendedGcd"
            [ test "extendedGcd example problem" <|
                \_ ->
                    extendedGcd 1215 465
                        |> Expect.equal ( 15, -13, 34 )

            -- , fuzz2 int int "extendedGcd fuzzy test" <|
            --     \a b ->
            --         let
            --             ( _, u, v ) =
            --                 extendedGcd a b
            --         in
            --         if a == 0 || b == 0 then
            --             extendedGcdTestChecker a b u v
            --                 |> Expect.false "extendedGcdTestChecker should return false"
            --         else
            --             extendedGcdTestChecker a b u v
            --                 |> Expect.true "extendedGcdTestChecker should return true"
            ]
        , describe "Arithmetic.powerMod"
            [ test "powerMod example test" <|
                \_ ->
                    powerMod 4147 8671 1000
                        |> Expect.equal 803
            ]
        , describe "Arithmetic.modularInverse"
            [ test "modularInverse example test 1" <|
                \_ ->
                    modularInverse 3 11
                        |> Expect.equal (Just 4)
            , test "modularInverse example test 2" <|
                \_ ->
                    modularInverse 3 15
                        |> Expect.equal Nothing
            ]
        , describe "Arithmetic.chineseRemainder"
            [ test "chineseRemainder example 1" <|
                \_ ->
                    chineseRemainder []
                        |> Expect.equal (Just 0)
            , test "chineseRemainder example 2" <|
                \_ ->
                    chineseRemainder [ ( 2, 3 ), ( 4, 6 ) ]
                        |> Expect.equal Nothing
            , test "chineseRemainder example 3" <|
                \_ ->
                    chineseRemainder [ ( 10, 11 ), ( 4, 12 ), ( 12, 13 ) ]
                        |> Expect.equal (Just 1000)
            ]
        , describe "Arithmetic.isPrime"
            [ test "isPrime filters out the first few primes" <|
                \_ ->
                    List.filter isPrime (List.range 1 100)
                        |> Expect.equalLists [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97 ]
            ]
        , describe "Arithmetic.primesBelow"
            [ test "primesBelow example 1" <|
                \_ ->
                    primesBelow 4
                        |> Expect.equal [ 2, 3 ]
            , test "primesBelow example 2" <|
                \_ ->
                    primesBelow 17
                        |> Expect.equal [ 2, 3, 5, 7, 11, 13 ]
            ]
        ]



-- HELPERS


getDigits : Int -> List Int
getDigits =
    List.reverse
        << unfoldr
            (\a ->
                if a == 0 then
                    Nothing

                else
                    Just ( modBy 10 a, a // 10 )
            )
        << abs


extendedGcdTestChecker : Int -> Int -> Int -> Int -> Bool
extendedGcdTestChecker a b u v =
    (abs u < b // gcd a b) && (abs v < a // gcd a b)


unfoldr : (b -> Maybe ( a, b )) -> b -> List a
unfoldr f seed =
    case f seed of
        Nothing ->
            []

        Just ( a, b ) ->
            a :: unfoldr f b
