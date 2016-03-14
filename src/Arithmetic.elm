module Arithmetic
    ( isEven, isOdd
    , toBase, fromBase
    , intSquareRoot, exactIntSquareRoot, isSquare
    , cbrt, intCubeRoot, exactIntCubeRoot, isCube
    , divides, isMultipleOf, divisors, properDivisors, numDivisors
    , gcd, lcm, isCoprimeTo, totient
    , powerMod
    , isPrime, primesBelow, primeFactors, primeExponents
    ) where

{-| A library that provides useful number-theoretical functions for dealing
with integers, primes, divisibility, et cetera.

# Primes
@docs isPrime, primesBelow, primeFactors, primeExponents

# Parity
@docs isEven, isOdd

# Divisors
@docs divides, isMultipleOf, divisors, properDivisors, numDivisors

# GCD and LCM
@docs gcd, lcm, isCoprimeTo, totient

# Base conversion
@docs toBase, fromBase

# Squares
@docs intSquareRoot, exactIntSquareRoot, isSquare

# Cubes
@docs cbrt, intCubeRoot, exactIntCubeRoot, isCube

# Modular arithmetic
@docs powerMod

-}

import Array


{- Parity -}

{-| Test whether an integer is even.

    isEven 2 == True
    isEven 3 == False
-}
isEven : Int -> Bool
isEven n =
    n % 2 == 0


{-| Test whether an integer is odd.

    isOdd 2 == False
    isOdd 3 == True
-}
isOdd : Int -> Bool
isOdd n =
    n % 2 /= 0


{- Base conversion -}

{-| Convert a number to a list of digits in the given base. The input number is
made positive first.

    toBase 2 42 = [1, 0, 1, 0, 1, 0]  -- 42 in binary
-}
toBase : Int -> Int -> List Int
toBase base n =
    let
        n' = abs n
        go x acc =
            if x <= 0 then
                acc
            else
                go (x // base) ((x % base) :: acc)
    in
        go n' []


{-| Interpret a list of digits as a number in the given base. The input is 
expected to consist of integers `d` for which `0 <= d < base`.

    fromBase 2 [1, 0, 1, 0, 1, 0] = 42
-}
fromBase : Int -> List Int -> Int
fromBase base =
    List.foldl (\x acc -> acc * base + x) 0


{- Squares -}

{-| Integer square root, rounding down.

    intSquareRoot 20 == 4
    intSquareRoot 25 == 5
-}
intSquareRoot : Int -> Int
intSquareRoot =
    toFloat >> sqrt >> round


{-| Integer square root returning `Nothing` if the given number is not a
square.

    exactIntSquareRoot 20 == Nothing
    exactIntSquareRoot 25 == Just 5
-}
exactIntSquareRoot : Int -> Maybe Int
exactIntSquareRoot n =
    let
        s = intSquareRoot n
    in
        if s * s == n then Just s else Nothing


{-| Test whether a number is a square.

    isSquare 20 == False
    isSquare 25 == True
-}
isSquare : Int -> Bool
isSquare n =
    let
        r = n % 48
    in
        (r == 0 || r == 1 || r == 4 || r == 9 || r == 16
                || r == 25 || r == 33 || r == 36) &&
            intSquareRoot n ^ 2 == n


{- Cubes -}

{-| Take the cube root of a number.

    cbrt 15.625 == 2.5
-}
cbrt : Float -> Float
cbrt n =
    n ^ (1/3)


{-| Integer cube root, rounding down.

    intCubeRoot 800 == 9
    intCubeRoot 1000 == 10
-}
intCubeRoot : Int -> Int
intCubeRoot =
    toFloat >> cbrt >> round


{-| Integer cube root, returning `Nothing` if the given number is not a cube.

    exactIntCubeRoot 800 == Nothing
    exactIntCubeRoot 1000 == Just 10
-}
exactIntCubeRoot : Int -> Maybe Int
exactIntCubeRoot n =
    let
        s = intCubeRoot n
    in
        if s ^ 3 == n then Just s else Nothing


{-| Test whether a number is a cube.

    isCube 800 == False
    isCube 1000 == True
-}
isCube : Int -> Bool
isCube n =
    let
        r = n % 63
    in
        (r == 0 || r == 1 || r == 8 || r == 27 || r == 28
                || r == 35 || r == 36 || r == 55 || r == 62) &&
            intCubeRoot n ^ 3 == n


{- Divisors -}

{-| Test whether one number divides another.

    10 `divides` 120 == True
    10 `divides` 125 == False
-}
divides : Int -> Int -> Bool
divides a b =
    b % a == 0


{-| Test whether one number is a multiple of another.

    120 `isMultipleOf` 10 == True
    125 `isMultipleOf` 10 == False
-}
isMultipleOf : Int -> Int -> Bool
isMultipleOf a b =
    a % b == 0


{-| Get all divisors of a number, in ascending order.

    divisors 20 == [1, 2, 4, 5, 10, 20]
-}
divisors : Int -> List Int
divisors =
    let
        f (p, e) =
            List.concatMap (\a -> List.map (\x -> p ^ x * a) [0..e])
    in
        primeExponents >> List.foldr f [1] >> List.sort


{-| Get all proper divisors (i.e., divisors less than the input) of a number,
in ascending order.

    properDivisors 20 == [1, 2, 4, 5, 10]
-}
properDivisors : Int -> List Int
properDivisors n =
    divisors n
    |> List.filter ((/=) n)


{-| Get the number of divisors of a number (counting itself).
-}
numDivisors : Int -> Int
numDivisors =
    primeExponents >> List.map (\(_, e) -> e + 1) >> List.product


{- GCD and LCM -}

{-| Calculate the greatest common divisor of two integers. `gcd x 0` and
`gcd 0 x` both return `x`. Negative arguments are made positive first.

    gcd 56 80 == 8
-}
gcd : Int -> Int -> Int
gcd a b =
    let
        gcd' a b = if b == 0 then a else gcd' b (a % b)
    in
        gcd' (abs a) (abs b)


{-| Calculate the least common multiple of two integers. `lcm x 0` and
`lcm 0 x` both return `0`. Negative arguments are made positive first.

    lcm 56 80 == 560
-}
lcm : Int -> Int -> Int
lcm a b =
    abs ((a // gcd a b) * b)


{-| Test whether two integers are coprime.

    56 `isCoprimeTo` 80 == False
    5 `isCoprimeTo` 8 
-}
isCoprimeTo : Int -> Int -> Bool
isCoprimeTo a b =
    gcd a b == 1


{-| Compute Euler's totient function `φ(n)`: the number of positive integers
`1 <= k <= n` for which `gcd(n, k) == 1`. The input is made positive first.

    totient 99 == 60
    totient 1450 == 560
-}
totient : Int -> Int
totient n =
    let
        n' = abs n
        f p n = n * (p - 1) // p
    in
        List.foldr f n' (List.map fst (primeExponents n'))


{- Modular arithmetic -}

{-| `powerMod b e m` efficiently calculates `b ^ e` (modulo `m`). It assumes
`b >= 0`, `e >= 0` and `m >= 1`.

For example, to compute `4147 ^ 8671 % 1000` efficiently:

    powerMod 4147 8671 1000 == 803
-}
powerMod : Int -> Int -> Int -> Int
powerMod base exponent modulus =
    let go b e r =
        if e == 0 then
            r
        else
            let
                r' = if isOdd e then (r * b) % modulus else r
            in
                go (b * b % modulus) (e // 2) r'
    in
        if modulus == 1 then 0 else go (base % modulus) exponent 1


{- Halve this number until it is odd. Then, return a tuple `(k, m)`, where
`k` is the number of times the input was halved, and `m` is the resulting odd
number. In other words, the original number equals `(2 ^ k) * m`.

    shiftToOdd 999 == (0, 999)
    shiftToOdd 1000 == (3, 125)
-}
shiftToOdd : Int -> (Int, Int)
shiftToOdd n =
    let
        f k m =
            if m % 2 == 1 then
                (k, m)
            else
                f (k + 1) (m // 2)
    in
        f 0 n


{- Primes -}

{-| Test whether an integer is a positive prime.

    isPrime 2357 == True
    isPrime 500 == False
-}
isPrime : Int -> Bool
isPrime n =
    if n < 13 then
        n == 2 || n == 3 || n == 5 || n == 7 || n == 11

    else if n % 2 == 0 || n % 3 == 0 || n % 5 == 0 then
        False

    else if n < 1373653 then
        millerRabin n [2, 3]

    else -- n < 2152302898747
        millerRabin n [2, 3, 5, 7, 11]


{-| Perform a Miller-Rabit pseudoprimality test on `n` with the given
witnesses, which should all be in the range `[2..n-2]`. Return `True` if the
integer is a probable prime, and `False` if it is definitely composite.

    millerRabin 2357 [2, 3] == True
    millerRabin 500 [2, 3] == False
-}
millerRabin : Int -> List Int -> Bool
millerRabin n witnesses =
    let
        (s, d) = shiftToOdd (n - 1)

        check l x =
            if l <= 0 then
                True
            else
                let
                    y = powerMod x 2 n
                in
                    y == 1 || (y /= n - 1 && check (l - 1) y)

        go witnesses =
            case witnesses of
                [] -> True
                a :: rest ->
                    let x = powerMod a d n in
                    if x == 1 || x == n - 1 then
                        go rest
                    else if check (s - 1) x then
                        False
                    else
                        go rest
    in
        go witnesses


{-| Get all primes in the given range `[0..n-1]`, using the Sieve of
Eratosthenes.

    primesBelow 4 == [2, 3]
    primesBelow 17 == [2, 3, 5, 7, 11, 13]
-}
primesBelow : Int -> List Int
primesBelow n =
    let
        ps = 2 :: List.map (\x -> 2 * x + 1) [1..intSquareRoot n // 2]

        initial =
            Array.repeat n True
            |> Array.set 0 False
            |> Array.set 1 False

        sieve p arr =
            let
                mark i p n arr =
                    if i * p >= n then
                        arr
                    else
                        mark (i + 1) p n (Array.set (i * p) False arr)
            in
                if Array.get p arr == Just True then
                    mark 2 p n arr
                else
                    arr

        trueIndices =
            let
                f i pred =
                    if pred then Just i else Nothing

                g x acc =
                    case x of
                        Just x -> x :: acc
                        Nothing -> acc
            in
                Array.indexedMap f >> Array.foldr g []
    in
        trueIndices (List.foldr sieve initial ps)


{-| Return a list of all prime factors for a given positive integer, in
ascending order. If the input is less than 2, the empty list is returned.

    primeFactors 24 == [2, 2, 2, 3]
    primeFactors 767 == [13, 59]
    primeFactors 1 == []
-}
primeFactors : Int -> List Int
primeFactors n =
    let
        go p n factors =
            if p * p > n then
                List.reverse factors ++ [n]

            else if n % p == 0 then
                go p (n // p) (p :: factors)

            else
                go (p + 1 + p % 2) n factors

    in
        if n <= 1 then [] else go 2 n []


{-| Return a list of all prime-exponent pairs for a given positive integer's
prime decomposition, with the primes in ascending order. If the input is less
than 2, the empty list is returned.

    primeExponents 24 == [(2, 3), (5, 2)]                -- 2^3 * 5^2
    primeExponents 531764 == [(2, 1), (11, 2), (13, 3)]  -- 2^1 * 11^2 * 13^3
    primeExponents 1 == []                               -- empty product
-}
primeExponents : Int -> List (Int, Int)
primeExponents =
    let
        runLengthCons x acc =
            case acc of
                [] -> [(x, 1)]
                ((y, n) :: rest) ->
                    if x == y then
                        ((y, n + 1) :: rest)
                    else
                        ((x, 1) :: (y, n) :: rest)
    in
        primeFactors >> List.foldr runLengthCons []
