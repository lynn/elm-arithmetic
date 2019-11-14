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
        , primeExponents
        , primeFactors
        , primesBelow
        , properDivisors
        , safeSquareRoot
        , toBase
        , totient
        )
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (float, int)
import Test exposing (Test, describe, fuzz, fuzz2, test)


suite : Test
suite =
    describe "Arithmetic module"
        [ describe "Arithmetic.isEven"
            [ test "isEven example test 1" <|
                \_ ->
                    isEven 2
                        |> Expect.true "isEven 2 should return True"
            , test "isEven example test 2" <|
                \_ ->
                    isEven 3
                        |> Expect.false "isEven 3 should return False"
            , test "isEven detects even numbers correctly" <|
                \_ ->
                    List.filter isEven (List.range 1 20)
                        |> Expect.equalLists [ 2, 4, 6, 8, 10, 12, 14, 16, 18, 20 ]
            ]
        , describe "Arithmetic.isOdd"
            [ test "isOdd example test 1" <|
                \_ ->
                    isOdd 2
                        |> Expect.false "isOdd 2 should return False"
            , test "isOdd example test 2" <|
                \_ ->
                    isOdd 3
                        |> Expect.true "isOdd 3 should return True"
            , test "isOdd detects odd numbers correctly" <|
                \_ ->
                    List.filter isOdd (List.range 1 20)
                        |> Expect.equalLists [ 1, 3, 5, 7, 9, 11, 13, 15, 17, 19 ]
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
            , test "fromBase converts from base 8 correctly" <|
                \_ ->
                    fromBase 8 [ 1, 5, 0, 4, 6 ]
                        |> Expect.equal 6694
            , fuzz int "fromBase does nothing on base 10 numbers" <|
                \fuzzInt ->
                    fromBase 10 (getDigits fuzzInt)
                        |> Expect.equal (abs fuzzInt)
            ]
        , describe "Arithmetic.safeSquareRoot"
            [ test "safeSquareRoot example test 1" <|
                \_ ->
                    safeSquareRoot 5.76
                        |> Expect.equal (Just 2.4)
            , test "safeSquareRoot example test 2" <|
                \_ ->
                    safeSquareRoot -1
                        |> Expect.equal Nothing
            , fuzz float "safeSquareRoot returns Nothing for negatives and Just num for positives" <|
                \fuzzFloat ->
                    if fuzzFloat < 0 then
                        safeSquareRoot fuzzFloat
                            |> Expect.equal Nothing

                    else
                        safeSquareRoot fuzzFloat
                            |> Expect.equal (Just (sqrt fuzzFloat))
            ]
        , describe "Arithmetic.intSquareRoot"
            [ test "intSquareRoot example test 1" <|
                \_ ->
                    intSquareRoot 20
                        |> Expect.equal 4
            , test "intSquareRoot example test 2" <|
                \_ ->
                    intSquareRoot 25
                        |> Expect.equal 5
            , test "intSquareRoot returns the correct value for 21" <|
                \_ ->
                    intSquareRoot 21
                        |> Expect.equal 5
            , test "intSquareRoot returns the correct value for 97" <|
                \_ ->
                    intSquareRoot 97
                        |> Expect.equal 10
            ]
        , describe "Arithmetic.exactIntSquareRoot"
            [ test "exactIntSquareRoot example test 1" <|
                \_ ->
                    exactIntSquareRoot 20
                        |> Expect.equal Nothing
            , test "exactIntSquareRoot example test 2" <|
                \_ ->
                    exactIntSquareRoot 25
                        |> Expect.equal (Just 5)
            , test "exactIntSquareRoot returns Nothing for 35" <|
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
            [ test "isSquare example test 1" <|
                \_ ->
                    isSquare 20
                        |> Expect.false "20 is not a square number"
            , test "isSquare example test 2" <|
                \_ ->
                    isSquare 25
                        |> Expect.true "25 is a square number"
            , test "isSquare 0 should return true" <|
                \_ ->
                    isSquare 0
                        |> Expect.true "Should return true"
            , test "isSquare filters the squares out" <|
                \_ ->
                    List.filter isSquare (List.range 1 256)
                        |> Expect.equalLists [ 1, 4, 9, 16, 25, 36, 49, 64, 81, 100, 121, 144, 169, 196, 225, 256 ]
            ]
        , describe "Arithmetic.cubeRoot"
            [ test "cubeRoot example test 1" <|
                \_ ->
                    cubeRoot 15.625
                        |> Expect.within testingFloatTolerance 2.5
            , fuzz float "cubeRoot returns correct values when fuzzily tested" <|
                \fuzzFloat ->
                    (cubeRoot <| abs fuzzFloat)
                        |> Expect.within testingFloatTolerance (abs fuzzFloat ^ (1 / 3))
            ]
        , describe "Arithmetic.intCubeRoot"
            [ test "intCubeRoot example test 1" <|
                \_ ->
                    intCubeRoot 800
                        |> Expect.equal 9
            , test "intCubeRoot example test 2" <|
                \_ ->
                    intCubeRoot 1000
                        |> Expect.equal 10
            , test "intCubeRoot returns NaN for negatives" <|
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
            [ test "exactIntCubeRoot example test 1" <|
                \_ ->
                    exactIntCubeRoot 800
                        |> Expect.equal Nothing
            , test "exactIntCubeRoot example test 2" <|
                \_ ->
                    exactIntCubeRoot 1000
                        |> Expect.equal (Just 10)
            , test "exactIntCubeRoot returns the exact cubes after mapping" <|
                \_ ->
                    List.map
                        (\x -> x ^ 3)
                        (List.filterMap exactIntCubeRoot (List.range 1 343))
                        |> Expect.equalLists [ 1, 8, 27, 64, 125, 216, 343 ]
            ]
        , describe "Arithmetic.isCube"
            [ test "isCube example test 1" <|
                \_ ->
                    isCube 800
                        |> Expect.false "isCube 800 should return False"
            , test "isCube example test 2" <|
                \_ ->
                    isCube 1000
                        |> Expect.true "isCube 1000 should return True"
            , test "isCube detects cubes properly" <|
                \_ ->
                    let
                        input =
                            125
                    in
                    isCube input
                        |> Expect.true ("Expected " ++ String.fromInt input ++ " to be a cube")
            ]
        , describe "Arithmetic.divides"
            [ test "divides example test 1" <|
                \_ ->
                    divides 10 120
                        |> Expect.true "divides 10 120 should return True"
            , test "divides example test 2" <|
                \_ ->
                    divides 10 125
                        |> Expect.false "divides 10 125 should return False"
            , test "divides returns true for correct input" <|
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
            [ test "divisors example test 1" <|
                \_ ->
                    divisors 20
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
            [ test "properDivisors example test 1" <|
                \_ ->
                    properDivisors 20
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
            [ test "divisorCount example test 1" <|
                \_ ->
                    divisorCount 20
                        |> Expect.equal 6
            , fuzz int "divisorCount correct for random input" <|
                \fuzzInt ->
                    divisorCount fuzzInt
                        |> Expect.equal (List.length (divisors fuzzInt))
            ]
        , describe "Arithmetic.gcd"
            [ test "gcd example test 1" <|
                \_ ->
                    gcd 56 80
                        |> Expect.equal 8
            , test "gcd should return 2" <|
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
            ]
        , describe "Arithmetic.lcm"
            [ test "lcm example test 1" <|
                \_ ->
                    lcm 56 80
                        |> Expect.equal 560
            , test "lcm should return 0" <|
                \_ ->
                    lcm 0 3
                        |> Expect.equal 0
            , test "lcm, again, should return 0" <|
                \_ ->
                    lcm 3 0
                        |> Expect.equal 0
            ]
        , describe "Arithmetic.isCoprimeTo"
            [ test "isCoprimeTo example 1" <|
                \_ ->
                    isCoprimeTo 56 80
                        |> Expect.false "Should not be coprime"
            , test "isCoprimeTo example 2" <|
                \_ ->
                    isCoprimeTo 5 8
                        |> Expect.true "Should be coprime"
            , test "isCoprimeTo 2 3 should return True" <|
                \_ ->
                    isCoprimeTo 2 3
                        |> Expect.true "Should be coprime"
            ]
        , describe "Arithmetic.totient"
            [ test "totient example test 1" <|
                \_ ->
                    totient 99
                        |> Expect.equal 60
            , test "totient example test 2" <|
                \_ ->
                    totient 1450
                        |> Expect.equal 560
            ]
        , describe "Arithmetic.extendedGcd"
            [ test "extendedGcd example problem 1" <|
                \_ ->
                    extendedGcd 1215 465
                        |> Expect.equal ( 15, -13, 34 )
            , test "extendedGcd 24 31 should return ( 1, -9, 7)" <|
                \_ ->
                    extendedGcd 24 31
                        |> Expect.equal ( 1, -9, 7 )
            , test "extendedGcd 0 0 should return ( 0, 1, 0)" <|
                \_ ->
                    extendedGcd 0 0
                        |> Expect.equal ( 0, 1, 0 )
            , fuzz2 int int "extendedGcd fuzzy test" <|
                \a b ->
                    let
                        ( _, u, v ) =
                            extendedGcd a b
                    in
                    extendedGcdTestChecker a b u v
                        |> Expect.true "extendedGcdTestChecker should return true"
            , fuzz2 int int "extendedGcd fuzzy test 2" <|
                \a b ->
                    let
                        ( d, u, v ) =
                            extendedGcd a b
                    in
                    ((a * u) + (b * v) == d && gcd a b == d)
                        |> Expect.true "should follow the au + bv = d and gcd a b = d identity"
            ]
        , describe "Arithmetic.powerMod"
            [ test "powerMod example test 1" <|
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
            [ test "isPrime example test 1" <|
                \_ ->
                    isPrime 2357
                        |> Expect.true "isPrime 2357 should return True"
            , test "isPrime example test 2" <|
                \_ ->
                    isPrime 500
                        |> Expect.false "isPrime 500 should return False"
            , test "isPrime filters out the first few primes" <|
                \_ ->
                    List.filter isPrime (List.range 1 100)
                        |> Expect.equalLists primesUnderOneHundred
            ]
        , describe "Arithmetic.primesBelow"
            [ test "primesBelow example test 1" <|
                \_ ->
                    primesBelow 4
                        |> Expect.equal [ 2, 3 ]
            , test "primesBelow example test 2" <|
                \_ ->
                    primesBelow 17
                        |> Expect.equal [ 2, 3, 5, 7, 11, 13 ]
            ]
        , describe "Arithmetic.primeFactors"
            [ test "primeFactors example test 1" <|
                \_ ->
                    primeFactors 24
                        |> Expect.equal [ 2, 2, 2, 3 ]
            , test "primeFactors example test 2" <|
                \_ ->
                    primeFactors 767
                        |> Expect.equal [ 13, 59 ]
            , test "primeFactors example test 3" <|
                \_ ->
                    primeFactors 1
                        |> Expect.equal []
            ]
        , describe "Arithmetic.primeExponents"
            [ test "primeExponents example test 1" <|
                \_ ->
                    primeExponents 24
                        |> Expect.equal [ ( 2, 3 ), ( 3, 1 ) ]
            , test "primeExponents example test 2" <|
                \_ ->
                    primeExponents 531764
                        |> Expect.equal [ ( 2, 2 ), ( 37, 1 ), ( 3593, 1 ) ]
            , test "primeExponents example test 3" <|
                \_ ->
                    primeExponents 1
                        |> Expect.equal []
            , fuzz int "primeExponents fuzz test" <|
                \fuzzInt ->
                    if fuzzInt < 2 then
                        primeExponents fuzzInt
                            |> Expect.equalLists []

                    else
                        fromPrimeExponentsToNumber (primeExponents fuzzInt)
                            |> Expect.equal fuzzInt
            ]
        ]



-- HELPERS


just : (expected -> actual -> Expectation) -> expected -> Maybe actual -> Expectation
just expect_ expectedValue actualMaybe =
    case actualMaybe of
        Just actualValue ->
            actualValue |> expect_ expectedValue

        Nothing ->
            Expect.fail "Expected a Just but got Nothing"


testingFloatTolerance : FloatingPointTolerance
testingFloatTolerance =
    Absolute 0.000001


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


unfoldr : (b -> Maybe ( a, b )) -> b -> List a
unfoldr f seed =
    case f seed of
        Nothing ->
            []

        Just ( a, b ) ->
            a :: unfoldr f b


primesUnderOneHundred : List Int
primesUnderOneHundred =
    [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97 ]



{- https://en.wikipedia.org/wiki/B%C3%A9zout%27s_identity#Structure_of_solutions -}


extendedGcdTestChecker : Int -> Int -> Int -> Int -> Bool
extendedGcdTestChecker a b u v =
    let
        eGcdTest x y =
            let
                absY =
                    abs y
            in
            abs x < absY || absY <= 1
    in
    eGcdTest u b && eGcdTest v a


fromPrimeExponentsToNumber : List ( Int, Int ) -> Int
fromPrimeExponentsToNumber nums =
    let
        raiseExponentTuple : ( Int, Int ) -> Int
        raiseExponentTuple ( x, y ) =
            x ^ y
    in
    List.product (List.map raiseExponentTuple nums)
