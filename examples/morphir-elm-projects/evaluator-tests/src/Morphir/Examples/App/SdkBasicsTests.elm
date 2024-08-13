module Morphir.Examples.App.SdkBasicsTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)
import Morphir.SDK.Int as Int exposing (Int64)


{-| Test: SdkBasics/add
Expected = 3
-}
sdkAddTest : TestContext -> Int
sdkAddTest ctx =
    test ctx <|
        let
            f x y =
                x + y
        in
        f 1 2


{-| Test: SdkBasics/add
Expected = 3
-}
sdkAddTest64 : { a : Int64, b : Int64 } -> Maybe Int64
sdkAddTest64 t =
    let
        f x y =
            let
                intX =
                    Int.fromInt64 x

                intY =
                    Int.fromInt64 y
            in
            intX + intY |> Int.toInt64
    in
    f t.a t.b


{-| Test: SdkBasics/intOverflow
Expected = 3

Scala's Int.MaxValue is (2^31) -1 = 2147483647
Elm's numerical ranges are platform-dependent
The values below will produce 36893488147419103000 from the JS engine

-}
sdkIntOverflowTest : TestContext -> Int
sdkIntOverflowTest ctx =
    test ctx <|
        let
            f x y =
                x + y
        in
        f (2 ^ 64) (2 ^ 64)


{-| Test: SdkBasics/subtract
Expected = 2
-}
sdkSubtractTest : TestContext -> Int
sdkSubtractTest ctx =
    test ctx <|
        let
            f x y =
                x - y
        in
        f 4 2


{-| Test: SdkBasics/subtract
expected(4, 2) = 2
-}
sdkSubtractTest64 : { a : Int64, b : Int64 } -> Maybe Int64
sdkSubtractTest64 t =
    let
        f x y =
            let
                xInt =
                    Int.fromInt64 x

                yInt =
                    Int.fromInt64 y
            in
            xInt - yInt |> Int.toInt64
    in
    f t.a t.b


{-| Test: SdkBasics/addFloat
Expected = 3.0
-}
sdkAddFloatTest : TestContext -> Float
sdkAddFloatTest ctx =
    test ctx <|
        let
            f x y =
                x + y
        in
        f 1.0 2.0


{-| Test: SdkBasics/floatOverflow
Expected = 3

Scala's Float.MaxValue is 3.4028235E38
Elm's numerical ranges are platform-dependent
The values below will produce 6.80564733841877e+38 from the JS engine

-}
sdkFloatOverflowTest : TestContext -> Float
sdkFloatOverflowTest ctx =
    test ctx <|
        let
            f x y =
                x + y
        in
        f (2.0 ^ 128) (2.0 ^ 128)


{-| Test: SdkBasics/subtractFloat
Expected = 2.0
-}
sdkSubtractFloatTest : TestContext -> Float
sdkSubtractFloatTest ctx =
    test ctx <|
        let
            f x y =
                x - y
        in
        f 4.0 2.0


{-| Test: SdkBasics/divide
Expected = 2.0
-}
sdkDivideTest : TestContext -> Float
sdkDivideTest ctx =
    test ctx <|
        let
            f x y =
                x / y
        in
        f 20.0 10.0


{-| Test: SdkBasics/divide - Int
Expected = 20
-}
sdkMultiplyIntTest : TestContext -> Int
sdkMultiplyIntTest ctx =
    test ctx <|
        let
            f x y =
                x * y
        in
        f 2 10


{-| Test: SdkBasics/multiplyFloat
Expected = 6.0
-}
sdkMultiplyFloatTest : TestContext -> Float
sdkMultiplyFloatTest ctx =
    test ctx <|
        let
            f x y =
                x * y
        in
        f 2.0 10.0


{-| Test: SdkBasics/divideByZero
Expected = Infinity
-}
sdkDivideByZeroTest : TestContext -> Float
sdkDivideByZeroTest ctx =
    test ctx <|
        let
            f x y =
                x / y
        in
        f 20.0 0


{-| Test: SdkBasics/toFloat
Expected = 2.0
-}
toFloatTest : TestContext -> Float
toFloatTest ctx =
    test ctx <|
        let
            f x =
                toFloat x
        in
        f 2


{-| Test: SdkBasics/round
Expected = 4
-}
sdkRoundTest : TestContext -> Int
sdkRoundTest ctx =
    test ctx <|
        let
            f x =
                round x
        in
        f 123.456


{-| Test: SdkBasics/round2
Expected = 4
-}
sdkRoundTest2 : TestContext -> Int
sdkRoundTest2 ctx =
    test ctx <|
        let
            f x =
                round x
        in
        f 123


{-| Test: SdkBasics/modBy
Expected = 2
-}
sdkModByTest : TestContext -> Int
sdkModByTest ctx =
    test ctx <|
        let
            f x y =
                modBy x y
        in
        f 3 20


{-| Test: SdkBasics/negate
Expected = -3
-}
sdkNegateTest : TestContext -> Int
sdkNegateTest ctx =
    test ctx <|
        let
            f x =
                negate x
        in
        f 3


{-| Test: SdkBasics/negate2
Expected = 3
-}
sdkNegateTest2 : TestContext -> Int
sdkNegateTest2 ctx =
    test ctx <|
        let
            f x =
                negate x
        in
        f -3


{-| Test: SdkBasics/abs
Expected = 3
-}
sdkAbsTest : TestContext -> Int
sdkAbsTest ctx =
    test ctx <|
        let
            f x =
                abs x
        in
        f -3


{-| Test: SdkBasics/abs2
Expected = 3
-}
sdkAbsTest2 : TestContext -> Int
sdkAbsTest2 ctx =
    test ctx <|
        let
            f x =
                abs x
        in
        f 3


{-| Test: SdkBasics/isNan
Expected = True
-}
sdkIsNaNTest : TestContext -> Bool
sdkIsNaNTest ctx =
    test ctx <|
        let
            f x =
                isNaN x
        in
        f (0 / 0)


{-| Test: SdkBasics/isInfinite
Expected = True
-}
sdkIsInfiniteTest : TestContext -> Bool
sdkIsInfiniteTest ctx =
    test ctx <|
        let
            f x =
                isInfinite x
        in
        f (1 / 0)


{-| Test: SdkBasics/sqrt
Expected = 4
-}
sdkSqrtTest : TestContext -> Float
sdkSqrtTest ctx =
    test ctx <|
        let
            f x =
                sqrt x
        in
        f 16


{-| Test: SdkBasics/logBase
Expected: 2
-}
sdkLogBaseTest : TestContext -> Float
sdkLogBaseTest ctx =
    test ctx <|
        let
            f x y =
                logBase x y
        in
        f 10 100


{-| Test: SdkBasics/logBase2
Expected: 8
-}
sdkLogBaseTest2 : TestContext -> Float
sdkLogBaseTest2 ctx =
    test ctx <|
        let
            f x y =
                logBase x y
        in
        f 2 256


{-| Test: SdkBasics/eulersNumber
Expected: 2.718281828459045
-}
sdkEulersNumberTest : TestContext -> Float
sdkEulersNumberTest ctx =
    test ctx <|
        let
            f =
                e
        in
        f


{-| Test: SdkBasics/pi
Expected: 3.141592653589793
-}
sdkPiTest : TestContext -> Float
sdkPiTest ctx =
    test ctx <|
        let
            f =
                pi
        in
        f


{-| Test: SdkBasics/cos
Expected: 0.5000000000000001
-}
sdkCosTest : TestContext -> Float
sdkCosTest ctx =
    test ctx <|
        let
            f x =
                cos x
        in
        f (degrees 60)


{-| Test: SdkBasics/sin
Expected: 0.8660254037844386
-}
sdkSinTest : TestContext -> Float
sdkSinTest ctx =
    test ctx <|
        let
            f x =
                sin x
        in
        f (degrees 60)


{-| Test: SdkBasics/tan
Expected: 0.9999999999999999
-}
sdkTanTest : TestContext -> Float
sdkTanTest ctx =
    test ctx <|
        let
            f x =
                tan x
        in
        f (degrees 45)


{-| Test: SdkBasics/acos
Expected: 1.0471975511965979
-}
sdkACosTest : TestContext -> Float
sdkACosTest ctx =
    test ctx <|
        let
            f x =
                acos x
        in
        f (1 / 2)


{-| Test: SdkBasics/asin
Expected: 0.5235987755982989
-}
sdkASinTest : TestContext -> Float
sdkASinTest ctx =
    test ctx <|
        let
            f x =
                asin x
        in
        f (1 / 2)


{-| Test: SdkBasics/atan
Expected: 0.7853981633974483
-}
sdkATanTest : TestContext -> Float
sdkATanTest ctx =
    test ctx <|
        let
            f x =
                atan x
        in
        f 1


{-| Test: SdkBasics/atan2
Expected: 0.7853981633974483
-}
sdkATan2Test : TestContext -> Float
sdkATan2Test ctx =
    test ctx <|
        let
            f x y =
                atan2 x y
        in
        f 1 1


{-| Test: SdkBasics/degrees
Expected: 3.141592653589793
-}
sdkDegreesTest : TestContext -> Float
sdkDegreesTest ctx =
    test ctx <|
        let
            f x =
                degrees x
        in
        f 180


{-| Test: SdkBasics/radians
Expected: 3.141592653589793
-}
sdkRadiansTest : TestContext -> Float
sdkRadiansTest ctx =
    test ctx <|
        let
            f x =
                radians x
        in
        f pi


{-| Test: SdkBasics/turns
Expected: 3.141592653589793
-}
sdkTurnsTest : TestContext -> Float
sdkTurnsTest ctx =
    test ctx <|
        let
            f x =
                turns x
        in
        f (1 / 2)


{-| Test: SdkBasics/toPolar
Expected: (5,0.9272952180016122)
-}
sdkToPolarTest : TestContext -> ( Float, Float )
sdkToPolarTest ctx =
    test ctx <|
        let
            f x =
                toPolar x
        in
        f ( 3, 4 )


{-| Test: SdkBasics/fromPolar
Expected: (1.2247448713915892, 0.7071067811865475)
-}
sdkFromPolarTest : TestContext -> ( Float, Float )
sdkFromPolarTest ctx =
    test ctx <|
        let
            f x =
                fromPolar x
        in
        f ( sqrt 2, degrees 30 )


{-| Test: SdkBasics/equal
Expected: True
-}
sdkEqualTest : TestContext -> Bool
sdkEqualTest ctx =
    test ctx <|
        let
            f x y =
                x == y
        in
        f 2 2


{-| Test: SdkBasics/equal2
Expected: True
-}
sdkEqualTest2 : TestContext -> Bool
sdkEqualTest2 ctx =
    test ctx <|
        let
            f x y =
                x == y
        in
        f 2.0 2.0


{-| Test: SdkBasics/equal3
Expected: True
-}
sdkEqualTest3 : TestContext -> Bool
sdkEqualTest3 ctx =
    test ctx <|
        let
            f x y =
                x == y
        in
        f True True


{-| Test: SdkBasics/equal4
Expected: True
-}
sdkEqualTest4 : TestContext -> Bool
sdkEqualTest4 ctx =
    test ctx <|
        let
            f x y =
                x == y
        in
        f 'a' 'a'


{-| Test: SdkBasics/equal5
Expected: True
-}
sdkEqualTest5 : TestContext -> Bool
sdkEqualTest5 ctx =
    test ctx <|
        let
            f x y =
                x == y
        in
        f "yo" "yo"


{-| Test: SdkBasics/equal6
Expected: True
-}
sdkEqualTest6 : TestContext -> Bool
sdkEqualTest6 ctx =
    test ctx <|
        let
            f x y =
                x == y
        in
        f [ 1, 2 ] [ 1, 2 ]


{-| Test: SdkBasics/equal7
Expected: True
-}
sdkEqualTest7 : TestContext -> Bool
sdkEqualTest7 ctx =
    test ctx <|
        let
            f x y =
                x == y
        in
        f ( 1, 2, 3 ) ( 1, 2, 3 )



-----


{-| Test: SdkBasics/notEqual
Expected: True
-}
sdkNotEqualTest : TestContext -> Bool
sdkNotEqualTest ctx =
    test ctx <|
        let
            f x y =
                x /= y
        in
        f 1 2


{-| Test: SdkBasics/notEqual2
Expected: True
-}
sdkNotEqualTest2 : TestContext -> Bool
sdkNotEqualTest2 ctx =
    test ctx <|
        let
            f x y =
                x /= y
        in
        f 1.0 2.0


{-| Test: SdkBasics/notEqual3
Expected: True
-}
sdkNotEqualTest3 : TestContext -> Bool
sdkNotEqualTest3 ctx =
    test ctx <|
        let
            f x y =
                x /= y
        in
        f False True


{-| Test: SdkBasics/notEqual4
Expected: True
-}
sdkNotEqualTest4 : TestContext -> Bool
sdkNotEqualTest4 ctx =
    test ctx <|
        let
            f x y =
                x /= y
        in
        f 'a' 'b'


{-| Test: SdkBasics/notEqual5
Expected: True
-}
sdkNotEqualTest5 : TestContext -> Bool
sdkNotEqualTest5 ctx =
    test ctx <|
        let
            f x y =
                x /= y
        in
        f "yo" "yoo"


{-| Test: SdkBasics/notEqual6
Expected: True
-}
sdkNotEqualTest6 : TestContext -> Bool
sdkNotEqualTest6 ctx =
    test ctx <|
        let
            f x y =
                x /= y
        in
        f [ 1, 2 ] [ 1, 2, 3 ]


{-| Test: SdkBasics/notEqual7
Expected: True
-}
sdkNotEqualTest7 : TestContext -> Bool
sdkNotEqualTest7 ctx =
    test ctx <|
        let
            f x y =
                x /= y
        in
        f ( 1, 2, 3 ) ( 1, 2, 4 )



-----


{-| Test: SdkBasics/lessThanInt
Expected: True
-}
sdkLessThanTestIntTrue : TestContext -> Bool
sdkLessThanTestIntTrue ctx =
    test ctx <|
        let
            f x y =
                x < y
        in
        f 2 4


{-| Test: SdkBasics/lessThanInt
Expected: False
-}
sdkLessThanTestIntFalse : TestContext -> Bool
sdkLessThanTestIntFalse ctx =
    test ctx <|
        let
            f x y =
                x < y
        in
        f 5 4


{-| Test: SdkBasics/greaterThanInt
Expected: True
-}
sdkGreaterThanTestIntTrue : TestContext -> Bool
sdkGreaterThanTestIntTrue ctx =
    test ctx <|
        let
            f x y =
                x > y
        in
        f 4 2


{-| Test: SdkBasics/greaterThanInt
Expected: False
-}
sdkGreaterThanTestIntFalse : TestContext -> Bool
sdkGreaterThanTestIntFalse ctx =
    test ctx <|
        let
            f x y =
                x > y
        in
        f 1 2


{-| Test: SdkBasics/greaterThanEqualsInt (1)
Expected: True
-}
sdkGreaterThanOrEqualTestIntTrue1 : TestContext -> Bool
sdkGreaterThanOrEqualTestIntTrue1 ctx =
    test ctx <|
        let
            f x y =
                x >= y
        in
        f 4 2


{-| Test: SdkBasics/greaterThanEqualsInt (2)
Expected: True
-}
sdkGreaterThanOrEqualTestIntTrue2 : TestContext -> Bool
sdkGreaterThanOrEqualTestIntTrue2 ctx =
    test ctx <|
        let
            f x y =
                x >= y
        in
        f 2 2


{-| Test: SdkBasics/greaterThanEqualsInt
Expected: False
-}
sdkGreaterThanOrEqualTestIntFalse : TestContext -> Bool
sdkGreaterThanOrEqualTestIntFalse ctx =
    test ctx <|
        let
            f x y =
                x >= y
        in
        f 1 2


{-| Test: SdkBasics/lessThanEqualsInt (1)
Expected: True
-}
sdkLessThanOrEqualTestIntTrue1 : TestContext -> Bool
sdkLessThanOrEqualTestIntTrue1 ctx =
    test ctx <|
        let
            f x y =
                x <= y
        in
        f 2 4


{-| Test: SdkBasics/lessThanEqualsInt (2)
Expected: True
-}
sdkLessThanOrEqualTestIntTrue2 : TestContext -> Bool
sdkLessThanOrEqualTestIntTrue2 ctx =
    test ctx <|
        let
            f x y =
                x <= y
        in
        f 2 2


{-| Test: SdkBasics/lessThanEqualsInt
Expected: False
-}
sdkLessThanOrEqualTestIntFalse : TestContext -> Bool
sdkLessThanOrEqualTestIntFalse ctx =
    test ctx <|
        let
            f x y =
                x <= y
        in
        f 2 1



-----


{-| Test: SdkBasics/orderToString
expected(-1) = "LT"
expected(1) = "GT"
expected(0) = "EQ"
-}
sdkOrderToStringTest : Order -> String
sdkOrderToStringTest a =
    case a of
        LT ->
            "LT"

        EQ ->
            "EQ"

        GT ->
            "GT"


{-| Test: SdkBasics/lessThan
expected(2.0, 4.0) = true
expected('a', 'b') = true
expected("AA", "BB") = true
expected((1,2), (2,3)) = true
expected(List(1,2), List(2,3)) = true
-}
sdkLessThanTest : comparable -> comparable -> Bool
sdkLessThanTest a b =
    a < b


{-| Test: SdkBasics/greaterThan
expected(2.0, 4.0) = false
expected('a', 'b') = false
expected("AA", "BB") = false
expected((1,2), (2,3)) = false
expected(List(1,2), List(2,3)) = false
-}
sdkGreaterThanTest : comparable -> comparable -> Bool
sdkGreaterThanTest a b =
    a > b


{-| Test: SdkBasics/lessThanOrEqual
expected(2.0, 4.0) = true
expected('a', 'b') = true
expected("AA", "BB") = true
expected((1,2), (2,3)) = true
expected(List(1,2), List(2,3)) = true
-}
sdkLessThanOrEqualTest : comparable -> comparable -> Bool
sdkLessThanOrEqualTest a b =
    a <= b


{-| Test: SdkBasics/greaterThanOrEqual
expected(2.0, 4.0) = false
expected('a', 'b') = false
expected("AA", "BB") = false
expected((1,2), (2,3)) = false
expected(List(1,2), List(2,3)) = false
-}
sdkGreaterThanOrEqualTest : comparable -> comparable -> Bool
sdkGreaterThanOrEqualTest a b =
    a >= b


{-| Test: SdkBasics/max
expected(2, 4) = 4
expected(2.0, 4.0) = 4.0
expected('a', 'b') = 'b'
expected("AA", "BB") = "BB"
expected((1,2), (2,3)) = (2,3)
expected(List(1,2), List(2,3)) = List(2,3)
-}
sdkMaxTest : comparable -> comparable -> comparable
sdkMaxTest a b =
    max a b


{-| Test: SdkBasics/min
expected(2, 4) = 2
expected(2.0, 4.0) = 2.0
expected('a', 'b') = 'a'
expected("AA", "BB") = "AA"
expected((1,2), (2,3)) = (1,2)
expected(List(1,2), List(2,3)) = List(1,2)
-}
sdkMinTest : comparable -> comparable -> comparable
sdkMinTest a b =
    min a b


{-| Test: SdkBasics/compare
expected(1, 2) = -1
expected(2.0, 1.0) = 1
expected('r', 'b') = 1
expected("Red", "Red") = 0
expected((1,0), (1,2)) = -1
expected(List(1,0), List(1,0,0)) = -1
expected(List(1,1), List(1,0,0)) = 1
expected(List((1,"Blue"), (1, "Green"), List((1, "Blue"), (1, "An utter lack of any color, even black or white") = 1
-}
sdkCompareTest : comparable -> comparable -> Order
sdkCompareTest a b =
    compare a b



-----


{-| Test: SdkBasics/not
Expected: False
-}
sdkNotTest : TestContext -> Bool
sdkNotTest ctx =
    test ctx <|
        let
            f x =
                not x
        in
        f True


{-| Test: SdkBasics/and
Expected: False
-}
sdkAndTest : TestContext -> Bool
sdkAndTest ctx =
    test ctx <|
        let
            f x y =
                x && y
        in
        f True False


{-| Test: SdkBasics/or
Expected: True
-}
sdkOrTest : TestContext -> Bool
sdkOrTest ctx =
    test ctx <|
        let
            f x y =
                x || y
        in
        f True False



-----


{-| Test: SdkBasics/appendString
Expected: "aa-bb"
-}
sdkAppendStringTest : TestContext -> String
sdkAppendStringTest ctx =
    test ctx <|
        let
            f x y =
                String.append x y
        in
        f "aa-" "bb"


{-| Test: SdkBasics/appendList
Expected: [1, 2, 3, 4]
-}
sdkAppendListTest : TestContext -> List Int
sdkAppendListTest ctx =
    test ctx <|
        let
            f x y =
                List.append x y
        in
        f [ 1, 2 ] [ 3, 4 ]



-----


{-| Test: SdkBasics/composeLeft
Expected: False
-}
sdkComposeLeftTest : TestContext -> Bool
sdkComposeLeftTest ctx =
    test ctx <|
        let
            f x y z =
                x << y << z

            g =
                f not not not
        in
        g True


{-| Test: SdkBasics/composeRight
Expected: 603
-}
sdkComposeRightTest : TestContext -> Int
sdkComposeRightTest ctx =
    test ctx <|
        let
            f x y z =
                x >> y >> z

            add5 x =
                x + 5

            mul100 x =
                x * 100

            add3 x =
                x + 3

            g =
                f add5 mul100 add3
        in
        g 1


{-| Test: SdkBasics/basicsCeilingTest
expected(3.88) = 4
expected(3.0) = 3
-}
basicsCeilingTest : Float -> Int
basicsCeilingTest x =
    ceiling x


{-| Test: SdkBasics/basicsFloorTest
expected(3.88) = 3
expected(3.0) = 3
-}
basicsFloorTest : Float -> Int
basicsFloorTest x =
    floor x


{-| Test: SdkBasics/basicsIntegerDivideTest
expected(12, 2) = 6
expected(12, 0) = 0
expected(-12, 7) = -1
-}
basicsIntegerDivideTest : Int -> Int -> Int
basicsIntegerDivideTest x y =
    x // y


{-| Test: SdkBasics/basicsAbsTest
expected(-5.0) = 5.0
-}
basicsAbsTest : Float -> Float
basicsAbsTest x =
    abs x


{-| Test: SdkBasics/basicsAlwaysTest
expected(0) = List(0)
expected(4.0) = List(4.0)
expected('z') = List('z')
expected("A") = List("A")
-}
basicsAlwaysTest : a -> List a
basicsAlwaysTest x =
    let
        f y =
            List.map (always x) y
    in
    f [ 4 ]


{-| Test: SdkBasics/basicsClampTest
expected(100, 200, 1000) = 200
expected(100.0, 200.0, 50.0) = 100.0
expected(100.0, 200.0, 100.0) = 100.0
expected(100.0, 200.0, 200.0) = 200.0
expected(100.0, 200.0, 150.0) = 150.0
-}
basicsClampTest : number -> number -> number -> number
basicsClampTest min max x =
    clamp min max x


{-| Test: SdkBasics/basicsIdentityTest
expected(4) = 4
expected(-5.0) = -5.0
expected('b') = 'b'
expected("BB") = "BB"
expected((2,3)) = (2,3)
expected(List(2,3)) = List(2,3)
-}
basicsIdentityTest : a -> a
basicsIdentityTest x =
    x


{-| Test: SdkBasics/basicsPowerTest
expected(4.0, 5.0) = 1024
expected(2, 5) = 1024
-}
basicsPowerTest : number -> number -> number
basicsPowerTest n x =
    n ^ x


{-| Test: SdkBasics/basicsRemainderByTest
expected(4, 21) = 1
expected(4, -21) = -1
expected(0, 4) = 0
-}
basicsRemainderByTest : Int -> Int -> Int
basicsRemainderByTest x y =
    remainderBy x y


{-| Test: SdkBasics/basicsSqrtTest
expected(9.0) = 3.0
-}
basicsSqrtTest : Float -> Float
basicsSqrtTest x =
    sqrt x


{-| Test: SdkBasics/basicsTruncateTest
expected(1.2) = 1
expected(-1.2) = -1
expected(0.4) = 0
expected(-0.4) = 0
-}
basicsTruncateTest : Float -> Int
basicsTruncateTest x =
    truncate x


{-| Test: SdkBasics/basicsXorTest
expected(true, true) = false
expected(true, false) = true
expected(false, true) = true
expected(false, false) = false
-}
basicsXorTest : Bool -> Bool -> Bool
basicsXorTest x y =
    xor x y


{-| Test: SdkBasics/basicsRoundTest
expected(1.6) = 2
expected(1.4) = 1
-}
basicsRoundTest : Float -> Int
basicsRoundTest v =
    round v
