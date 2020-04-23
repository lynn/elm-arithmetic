module Arithmetic exposing
    ( isPrime, primesBelow, primeFactors, primeExponents
    , isEven, isOdd
    , divides, divisors, properDivisors, divisorCount
    , gcd, lcm, isCoprimeTo, totient, extendedGcd
    , toBase, fromBase
    , safeSquareRoot, intSquareRoot, exactIntSquareRoot, isSquare
    , cubeRoot, intCubeRoot, exactIntCubeRoot, isCube
    , powerMod, modularInverse, chineseRemainder
    )

{-| A library that provides useful number-theoretical functions for dealing
with integers, primes, divisibility, et cetera.


# Primes

@docs isPrime, primesBelow, primeFactors, primeExponents


# Parity

@docs isEven, isOdd


# Divisors

@docs divides, isMultipleOf, divisors, properDivisors, divisorCount


# GCD and LCM

@docs gcd, lcm, isCoprimeTo, totient, extendedGcd


# Base conversion

@docs toBase, fromBase


# Squares

@docs safeSquareRoot, intSquareRoot, exactIntSquareRoot, isSquare


# Cubes

@docs cubeRoot, intCubeRoot, exactIntCubeRoot, isCube


# Modular arithmetic

@docs powerMod, modularInverse, chineseRemainder

-}

import Array



{- Parity -}


{-| Test whether an integer is even.

    isEven 2 == True

    isEven 3 == False

-}
isEven : Int -> Bool
isEven =
    divides 2


{-| Test whether an integer is odd.

    isOdd 2 == False

    isOdd 3 == True

-}
isOdd : Int -> Bool
isOdd =
    not << isEven



{- Base conversion -}


{-| Convert an integer to a list of digits in the given base. The input integer is
made positive first.


    toBase 2 42 =
        [ 1, 0, 1, 0, 1, 0 ]

    -- 42 in binary

-}
toBase : Int -> Int -> List Int
toBase base n =
    let
        absN =
            abs n

        go x acc =
            if x <= 0 then
                acc

            else
                go (x // base) (modBy base x :: acc)
    in
    go absN []


{-| Interpret a list of digits as an integer in the given base. The input is
expected to consist of integers `d` for which `0 <= d < base`.

    fromBase 2 [ 1, 0, 1, 0, 1, 0 ] =
        42

-}
fromBase : Int -> List Int -> Int
fromBase base =
    List.foldl (\x acc -> acc * base + x) 0



{- Squares -}


{-| Safely take the square root of a float: return `Just (sqrt n)` if
the input `n` is nonnegative; otherwise, return `Nothing`.

    sqrt 5.76 == Just 2.4

    sqrt -1 == Nothing

-}
safeSquareRoot : Float -> Maybe Float
safeSquareRoot n =
    if n < 0 then
        Nothing

    else
        Just (sqrt n)


{-| Take the square root, rounding down. Return `NaN` (not a number) for
negative arguments.

    intSquareRoot 20 == 4

    intSquareRoot 25 == 5

-}
intSquareRoot : Int -> Int
intSquareRoot =
    toFloat >> sqrt >> round


{-| Return `Just s` if the given integer is a square, and `s` is its square
root; otherwise, return `Nothing`.

    exactIntSquareRoot 20 == Nothing

    exactIntSquareRoot 25 == Just 5

-}
exactIntSquareRoot : Int -> Maybe Int
exactIntSquareRoot n =
    let
        s =
            intSquareRoot n
    in
    if s * s == n then
        Just s

    else
        Nothing


{-| Test whether an integer is a square.

    isSquare 20 == False

    isSquare 25 == True

-}
isSquare : Int -> Bool
isSquare n =
    let
        r =
            modBy 48 n
    in
    ((r == 0)
        || (r == 1)
        || (r == 4)
        || (r == 9)
        || (r == 16)
        || (r == 25)
        || (r == 33)
        || (r == 36)
    )
        && (intSquareRoot n ^ 2 == n)



{- Cubes -}


{-| Take the cube root of a float.

    cubeRoot 15.625 == 2.5

-}
cubeRoot : Float -> Float
cubeRoot n =
    n ^ (1 / 3)


{-| Integer cube root, rounding down.

    intCubeRoot 800 == 9

    intCubeRoot 1000 == 10

-}
intCubeRoot : Int -> Int
intCubeRoot =
    toFloat >> cubeRoot >> round


{-| Return `Just s` if the given integer is a cube, and `s` is its cube root;
otherwise, return `Nothing`.

    exactIntCubeRoot 800 == Nothing

    exactIntCubeRoot 1000 == Just 10

-}
exactIntCubeRoot : Int -> Maybe Int
exactIntCubeRoot n =
    let
        s =
            intCubeRoot n
    in
    if s ^ 3 == n then
        Just s

    else
        Nothing


{-| Test whether an integer is a cube.

    isCube 800 == False

    isCube 1000 == True

-}
isCube : Int -> Bool
isCube n =
    let
        r =
            modBy 63 n
    in
    ((r == 0)
        || (r == 1)
        || (r == 8)
        || (r == 27)
        || (r == 28)
        || (r == 35)
        || (r == 36)
        || (r == 55)
        || (r == 62)
    )
        && (intCubeRoot n ^ 3 == n)



{- Divisors -}


{-| Test whether one integer divides another.

    divides 10 120 == True

    divides 10 125 == False

-}
divides : Int -> Int -> Bool
divides a b =
    modBy a b == 0


{-| Get all divisors of an integer, in ascending order.

    divisors 20 == [ 1, 2, 4, 5, 10, 20 ]

-}
divisors : Int -> List Int
divisors =
    let
        f ( p, e ) =
            List.concatMap (\a -> List.map (\x -> p ^ x * a) (List.range 0 e))
    in
    primeExponents >> List.foldr f [ 1 ] >> List.sort


{-| Get all proper divisors (i.e., divisors less than the input) of an integer,
in ascending order.

    properDivisors 20 == [ 1, 2, 4, 5, 10 ]

-}
properDivisors : Int -> List Int
properDivisors n =
    divisors n
        |> List.filter ((/=) n)


{-| Get the number of divisors of an integer (counting itself).

    divisorCount 20 =
        6

-}
divisorCount : Int -> Int
divisorCount =
    primeExponents >> List.map (\( _, e ) -> e + 1) >> List.product



{- GCD and LCM -}


{-| Calculate the greatest common divisor of two integers. `gcd x 0` and
`gcd 0 x` both return `x`. Negative arguments are made positive first.

    gcd 56 80 == 8

-}
gcd : Int -> Int -> Int
gcd a b =
    let
        gcd_ x y =
            if y == 0 then
                x

            else
                gcd_ y (modBy y x)
    in
    gcd_ (abs a) (abs b)


{-| Calculate the least common multiple of two integers. `lcm x 0` and
`lcm 0 x` both return `0`. Negative arguments are made positive first.

    lcm 56 80 == 560

-}
lcm : Int -> Int -> Int
lcm a b =
    abs ((a // gcd a b) * b)


{-| Test whether two integers are coprime.

    isCoprimeTo 56 80 == False

    isCoprimeTo 5 8 == True

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
        absN =
            abs n

        f p x =
            x * (p - 1) // p
    in
    List.foldr f absN (List.map Tuple.first (primeExponents absN))


{-| Given `a` and `b`, compute integers `(d, u, v)` so that `a * u + b * v ==
d` where `d == gcd a b`. (These are known as [Bézout coefficients](
<https://en.wikipedia.org/wiki/B%C3%A9zout%27s_identity>). If the inputs are both
positive, the solution returned satisfies `abs u <= b // gcd a b` and
`abs v <= a // gcd a b`.)

    extendedGcd 1215 465 == (15, -13, 34)
        -- because gcd 1215 465 == 15 == -13 * 1215 + 34 * 465

-}
extendedGcd : Int -> Int -> ( Int, Int, Int )
extendedGcd a b =
    let
        egcd n1 o1 n2 o2 r s =
            if s == 0 then
                ( r, o1, o2 )

            else
                let
                    q =
                        r // s
                in
                egcd (o1 - q * n1) n1 (o2 - q * n2) n2 s (remainderBy s r)

        ( d, x, y ) =
            egcd 0 1 1 0 (abs a) (abs b)

        u =
            if a < 0 then
                negate x

            else
                x

        v =
            if b < 0 then
                negate y

            else
                y
    in
    ( d, u, v )



{- Modular arithmetic -}


{-| `powerMod b e m` efficiently calculates `b ^ e` (modulo `m`). It assumes
`b >= 0`, `e >= 0` and `m >= 1`.

For example, to compute `4147 ^ 8671 % 1000` efficiently:

    powerMod 4147 8671 1000 == 803

-}
powerMod : Int -> Int -> Int -> Int
powerMod base exponent modulus =
    let
        go b e r =
            if e == 0 then
                r

            else
                let
                    r_ =
                        if isOdd e then
                            modBy modulus (r * b)

                        else
                            r
                in
                go (modBy modulus (b * b)) (e // 2) r_
    in
    if modulus == 1 then
        0

    else
        go (modBy modulus base) exponent 1



{- Halve this integer until it is odd. Then, return a tuple `(k, m)`, where
   `k` is the number of times the input was halved, and `m` is the resulting odd
   integer. In other words, the original integer equals `(2 ^ k) * m`.

       shiftToOdd 999 == (0, 999)
       shiftToOdd 1000 == (3, 125)

-}


shiftToOdd : Int -> ( Int, Int )
shiftToOdd n =
    let
        f k m =
            if modBy 2 m == 1 then
                ( k, m )

            else
                f (k + 1) (m // 2)
    in
    f 0 n


{-| Given an integer `a` and a modulus `n`, return the multiplicative inverse of
`a` modulo `n`, if it exists. That is: try to return `Just b`, with
`0 <= b < n`, so that `a * b == 1` modulo `n`, but return `Nothing` if no such
`b` exists. (`b` exists precisely when `a` and the modulus `n` are coprime.)

    modularInverse 3 11 == Just 4 -- 3 * 4 == 12 == 1 (mod 11)

    modularInverse 3 15 == Nothing -- 3 and 15 aren't coprime

-}
modularInverse : Int -> Int -> Maybe Int
modularInverse a modulus =
    let
        ( d, x, _ ) =
            extendedGcd a modulus
    in
    if d == 1 then
        Just (modBy modulus x)

    else
        Nothing


{-| Given a list of residue-modulus pairs `[(r1, m1), (r2, m2), ...]`, solve
the system of linear congruences:

    x = r1 (mod m1)
    x = r2 (mod m2)
    ...

Let `M` be the product of all moduli in the list. The [Chinese remainder
theorem](https://en.wikipedia.org/wiki/Chinese_remainder_theorem) tells us that

  - if all of the moduli are pairwise coprime (their `gcd` is 1), there are
    infinitely many solutions, all congruent `mod M`;
  - if there is a pair of non-coprime moduli in the list, there is no solution.

The result is a solution `Just x` with `0 <= x < M` in the first case; or
`Nothing` if the system is unsolvable.

    chineseRemainder [] == Just 0
        -- The trivial solution, modulo M = 1.

    chineseRemainder [(2, 3), (4, 6)] == Nothing
        -- 3 and 6 are not coprime, so there is no solution.

    chineseRemainder [(10, 11), (4, 12), (12, 13)] == Just 1000
        -- Solution to x = 10 (mod 11), x = 4 (mod 12), x = 12 (mod 13).

-}
chineseRemainder : List ( Int, Int ) -> Maybe Int
chineseRemainder equations =
    let
        ( residues, moduli ) =
            List.unzip equations

        m =
            List.product moduli

        v =
            List.map (\x -> m // x) moduli

        fromJustCons : Maybe a -> Maybe (List a) -> Maybe (List a)
        fromJustCons x acc =
            case x of
                Just y ->
                    Maybe.map ((::) y) acc

                Nothing ->
                    Nothing

        fromJustList =
            List.foldr fromJustCons (Just [])
    in
    fromJustList (List.map2 modularInverse v moduli)
        |> Maybe.map
            (List.map2 (*) residues
                >> List.map2 (*) v
                >> List.sum
                >> (\a -> (\dividend modulus -> modBy modulus dividend) a m)
            )



{- Primes -}


{-| Test whether an integer is a positive prime.

    isPrime 2357 == True

    isPrime 500 == False

-}
isPrime : Int -> Bool
isPrime n =
    if n < 13 then
        n == 2 || n == 3 || n == 5 || n == 7 || n == 11

    else if modBy 2 n == 0 || modBy 3 n == 0 || modBy 5 n == 0 then
        False

    else if n < 1373653 then
        millerRabin n [ 2, 3 ]

    else
        -- n < 2152302898747
        millerRabin n [ 2, 3, 5, 7, 11 ]


{-| Perform a Miller-Rabin pseudoprimality test on `n` with the given
witnesses, which should all be in the range `[2..n-2]`. Return `True` if the
integer is a probable prime, and `False` if it is definitely composite.

    millerRabin 2357 [ 2, 3 ] == True

    millerRabin 500 [ 2, 3 ] == False

-}
millerRabin : Int -> List Int -> Bool
millerRabin n witnessesList =
    let
        ( s, d ) =
            shiftToOdd (n - 1)

        check l x =
            if l <= 0 then
                True

            else
                let
                    y =
                        powerMod x 2 n
                in
                y == 1 || (y /= n - 1 && check (l - 1) y)

        go witnesses =
            case witnesses of
                [] ->
                    True

                a :: rest ->
                    let
                        x =
                            powerMod a d n
                    in
                    if x == 1 || x == n - 1 then
                        go rest

                    else if check (s - 1) x then
                        False

                    else
                        go rest
    in
    go witnessesList


{-| Get all primes in the given range `[0..n-1]`, using the Sieve of
Eratosthenes.

    primesBelow 4 == [ 2, 3 ]

    primesBelow 17 == [ 2, 3, 5, 7, 11, 13 ]

-}
primesBelow : Int -> List Int
primesBelow n =
    let
        ps =
            2 :: List.map (\x -> 2 * x + 1) (List.range 1 (intSquareRoot n // 2))

        initial =
            Array.repeat n True
                |> Array.set 0 False
                |> Array.set 1 False

        sieve p arr =
            let
                mark i x num array =
                    if i * x >= num then
                        array

                    else
                        mark (i + 1) x num (Array.set (i * x) False array)
            in
            if Array.get p arr == Just True then
                mark 2 p n arr

            else
                arr

        trueIndices =
            let
                f i pred =
                    if pred then
                        Just i

                    else
                        Nothing

                g x acc =
                    case x of
                        Just a ->
                            a :: acc

                        Nothing ->
                            acc
            in
            Array.indexedMap f >> Array.foldr g []
    in
    trueIndices (List.foldr sieve initial ps)


{-| Return a list of all prime factors for a given positive integer, in
ascending order. If the input is less than 2, the empty list is returned.

    primeFactors 24 == [ 2, 2, 2, 3 ]

    primeFactors 767 == [ 13, 59 ]

    primeFactors 1 == []

-}
primeFactors : Int -> List Int
primeFactors n =
    let
        go p num factors =
            if p * p > num then
                List.reverse factors ++ [ num ]

            else if modBy p num == 0 then
                go p (num // p) (p :: factors)

            else
                go (p + 1 + modBy 2 p) num factors
    in
    if n <= 1 then
        []

    else
        go 2 n []


{-| Return a list of all prime-exponent pairs for a given positive integer's
prime decomposition, with the primes in ascending order. If the input is less
than 2, the empty list is returned.

    primeExponents 24 == [ ( 2, 3 ), ( 3, 1 ) ] -- 2^3 * 3^1

    primeExponents 531764 == [ ( 2, 2 ), ( 37, 1 ), ( 3593, 1 ) ] -- 2^2 * 37^1 * 37^1

    primeExponents 1 == [] -- empty product

-}
primeExponents : Int -> List ( Int, Int )
primeExponents =
    let
        runLengthCons x acc =
            case acc of
                [] ->
                    [ ( x, 1 ) ]

                ( y, n ) :: rest ->
                    if x == y then
                        ( y, n + 1 ) :: rest

                    else
                        ( x, 1 ) :: ( y, n ) :: rest
    in
    primeFactors >> List.foldr runLengthCons []
