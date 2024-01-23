module Morphir.Examples.App.SdkBasicsTests exposing (..)

import Morphir.Examples.App.TestUtils exposing (..)
import Morphir.SDK.Basics exposing (integerDivide, power)
import Morphir.SDK.Int exposing (Int64)


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
sdkAddTest64 : { a : Int64, b : Int64 } -> Int64
sdkAddTest64 t =
    let
        f x y =
            x + y
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
Expected = 2
-}
sdkSubtractTest64 : { a : Int64, b : Int64 } -> Int64
sdkSubtractTest64 t =
    let
        f x y =
            x - y
    in
    f t.a t.b


{-| Test: SdkBasics/multiply
Expected = 6
-}
sdkMultiplyTest : TestContext -> Int
sdkMultiplyTest ctx =
    test ctx <|
        let
            f x y =
                x * y
        in
        f 2 3


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


{-| Test: SdkBasics/integerDivide
Expected = 2
-}
sdkIntegerDivideTest : TestContext -> Int
sdkIntegerDivideTest ctx =
    test ctx <|
        let
            f x y =
                x // y
        in
        f 20 10


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


{-| Test: SdkBasics/power
Expected = 16
-}
sdkPowerTest : TestContext -> Int
sdkPowerTest ctx =
    test ctx <|
        let
            f x y =
                x ^ y
        in
        f 4 2


{-| Test: SdkBasics/powerFloat
Expected = 16
-}
sdkPowerFloatTest : TestContext -> Float
sdkPowerFloatTest ctx =
    test ctx <|
        let
            f x y =
                x ^ y
        in
        f 4 2


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


{-| Test: SdkBasics/floor
Expected = 5
-}
sdkFloorTest : TestContext -> Int
sdkFloorTest ctx =
    test ctx <|
        let
            f x =
                floor x
        in
        f 5.75


{-| Test: SdkBasics/floor2
Expected = 5
-}
sdkFloorTest2 : TestContext -> Int
sdkFloorTest2 ctx =
    test ctx <|
        let
            f x =
                floor x
        in
        f 5


{-| Test: SdkBasics/ceiling
Expected = 8
-}
sdkCeilingTest : TestContext -> Int
sdkCeilingTest ctx =
    test ctx <|
        let
            f x =
                ceiling x
        in
        f 7.25


{-| Test: SdkBasics/ceiling2
Expected = 8
-}
sdkCeilingTest2 : TestContext -> Int
sdkCeilingTest2 ctx =
    test ctx <|
        let
            f x =
                ceiling x
        in
        f 8


{-| Test: SdkBasics/truncate
Expected = 5
-}
sdkTruncateTest : TestContext -> Int
sdkTruncateTest ctx =
    test ctx <|
        let
            f x =
                truncate x
        in
        f 5.25


{-| Test: SdkBasics/truncate2
Expected = 5
-}
sdkTruncateTest2 : TestContext -> Int
sdkTruncateTest2 ctx =
    test ctx <|
        let
            f x =
                truncate x
        in
        f 5


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


{-| Test: SdkBasics/remainderBy
Expected = 2
-}
sdkRemainderByTest : TestContext -> Int
sdkRemainderByTest ctx =
    test ctx <|
        let
            f x y =
                remainderBy x y
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


{-| Test: SdkBasics/clamp
Expected = 100
-}
sdkClampTest : TestContext -> Int
sdkClampTest ctx =
    test ctx <|
        let
            f x y z =
                clamp x y z
        in
        f 100 200 50


{-| Test: SdkBasics/clamp2
Expected = 100
-}
sdkClampTest2 : TestContext -> Int
sdkClampTest2 ctx =
    test ctx <|
        let
            f x y z =
                clamp x y z
        in
        f 100 200 100


{-| Test: SdkBasics/clamp3
Expected = 200
-}
sdkClampTest3 : TestContext -> Int
sdkClampTest3 ctx =
    test ctx <|
        let
            f x y z =
                clamp x y z
        in
        f 100 200 201


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
Expected: ~(1, 1)
-}
sdkFromPolarTest : TestContext -> ( Float, Float )
sdkFromPolarTest ctx =
    test ctx <|
        let
            f x =
                fromPolar x
        in
        f ( sqrt 2, degrees 45 )



-----


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


{-| Test: SdkBasics/lessThanFloat
Expected: True
-}
sdkLessThanTestFloat : TestContext -> Bool
sdkLessThanTestFloat ctx =
    test ctx <|
        let
            f x y =
                x < y
        in
        f 2.0 4.0


{-| Test: SdkBasics/lessThanChar
Expected: True
-}
sdkLessThanTestChar : TestContext -> Bool
sdkLessThanTestChar ctx =
    test ctx <|
        let
            f x y =
                x < y
        in
        f 'a' 'b'


{-| Test: SdkBasics/lessThanString
Expected: True
-}
sdkLessThanTestString : TestContext -> Bool
sdkLessThanTestString ctx =
    test ctx <|
        let
            f x y =
                x < y
        in
        f "AA" "BB"


{-| Test: SdkBasics/lessThanTuple
Expected: True
-}
sdkLessThanTestTuple : TestContext -> Bool
sdkLessThanTestTuple ctx =
    test ctx <|
        let
            f x y =
                x < y
        in
        f ( 1, 2 ) ( 2, 3 )


{-| Test: SdkBasics/lessThanList
Expected: True
-}
sdkLessThanTestList : TestContext -> Bool
sdkLessThanTestList ctx =
    test ctx <|
        let
            f x y =
                x < y
        in
        f [ 1, 2 ] [ 2, 3 ]



-----


{-| Test: SdkBasics/lessThanOrEqualInt
Expected: True
-}
sdkLessThanOrEqualTestInt : TestContext -> Bool
sdkLessThanOrEqualTestInt ctx =
    test ctx <|
        let
            f x y =
                x <= y
        in
        f 2 4


{-| Test: SdkBasics/lessThanOrEqualFloat
Expected: True
-}
sdkLessThanOrEqualTestFloat : TestContext -> Bool
sdkLessThanOrEqualTestFloat ctx =
    test ctx <|
        let
            f x y =
                x <= y
        in
        f 2.0 4.0


{-| Test: SdkBasics/lessThanOrEqualChar
Expected: True
-}
sdkLessThanOrEqualTestChar : TestContext -> Bool
sdkLessThanOrEqualTestChar ctx =
    test ctx <|
        let
            f x y =
                x <= y
        in
        f 'a' 'b'


{-| Test: SdkBasics/lessThanOrEqualString
Expected: True
-}
sdkLessThanOrEqualTestString : TestContext -> Bool
sdkLessThanOrEqualTestString ctx =
    test ctx <|
        let
            f x y =
                x <= y
        in
        f "AA" "BB"


{-| Test: SdkBasics/lessThanOrEqualTuple
Expected: True
-}
sdkLessThanOrEqualTestTuple : TestContext -> Bool
sdkLessThanOrEqualTestTuple ctx =
    test ctx <|
        let
            f x y =
                x <= y
        in
        f ( 1, 2 ) ( 2, 3 )


{-| Test: SdkBasics/lessThanOrEqualList
Expected: True
-}
sdkLessThanOrEqualTestList : TestContext -> Bool
sdkLessThanOrEqualTestList ctx =
    test ctx <|
        let
            f x y =
                x <= y
        in
        f [ 1, 2 ] [ 2, 3 ]



-----


{-| Test: SdkBasics/greaterThanInt
Expected: False
-}
sdkGreaterThanTestInt : TestContext -> Bool
sdkGreaterThanTestInt ctx =
    test ctx <|
        let
            f x y =
                x > y
        in
        f 2 4


{-| Test: SdkBasics/greaterThanFloat
Expected: False
-}
sdkGreaterThanTestFloat : TestContext -> Bool
sdkGreaterThanTestFloat ctx =
    test ctx <|
        let
            f x y =
                x > y
        in
        f 2.0 4.0


{-| Test: SdkBasics/greaterThanChar
Expected: False
-}
sdkGreaterThanTestChar : TestContext -> Bool
sdkGreaterThanTestChar ctx =
    test ctx <|
        let
            f x y =
                x > y
        in
        f 'a' 'b'


{-| Test: SdkBasics/greaterThanString
Expected: False
-}
sdkGreaterThanTestString : TestContext -> Bool
sdkGreaterThanTestString ctx =
    test ctx <|
        let
            f x y =
                x > y
        in
        f "AA" "BB"


{-| Test: SdkBasics/greaterThanTuple
Expected: False
-}
sdkGreaterThanTestTuple : TestContext -> Bool
sdkGreaterThanTestTuple ctx =
    test ctx <|
        let
            f x y =
                x > y
        in
        f ( 1, 2 ) ( 2, 3 )


{-| Test: SdkBasics/greaterThanList
Expected: False
-}
sdkGreaterThanTestList : TestContext -> Bool
sdkGreaterThanTestList ctx =
    test ctx <|
        let
            f x y =
                x > y
        in
        f [ 1, 2 ] [ 2, 3 ]



-----


{-| Test: SdkBasics/greaterThanOrEqualInt
Expected: False
-}
sdkGreaterThanOrEqualTestInt : TestContext -> Bool
sdkGreaterThanOrEqualTestInt ctx =
    test ctx <|
        let
            f x y =
                x >= y
        in
        f 2 4


{-| Test: SdkBasics/greaterThanOrEqualFloat
Expected: False
-}
sdkGreaterThanOrEqualTestFloat : TestContext -> Bool
sdkGreaterThanOrEqualTestFloat ctx =
    test ctx <|
        let
            f x y =
                x >= y
        in
        f 2.0 4.0


{-| Test: SdkBasics/greaterThanOrEqualChar
Expected: False
-}
sdkGreaterThanOrEqualTestChar : TestContext -> Bool
sdkGreaterThanOrEqualTestChar ctx =
    test ctx <|
        let
            f x y =
                x >= y
        in
        f 'a' 'b'


{-| Test: SdkBasics/greaterThanOrEqualString
Expected: False
-}
sdkGreaterThanOrEqualTestString : TestContext -> Bool
sdkGreaterThanOrEqualTestString ctx =
    test ctx <|
        let
            f x y =
                x >= y
        in
        f "AA" "BB"


{-| Test: SdkBasics/greaterThanOrEqualTuple
Expected: False
-}
sdkGreaterThanOrEqualTestTuple : TestContext -> Bool
sdkGreaterThanOrEqualTestTuple ctx =
    test ctx <|
        let
            f x y =
                x >= y
        in
        f ( 1, 2 ) ( 2, 3 )


{-| Test: SdkBasics/greaterThanOrEqualList
Expected: False
-}
sdkGreaterThanOrEqualTestList : TestContext -> Bool
sdkGreaterThanOrEqualTestList ctx =
    test ctx <|
        let
            f x y =
                x >= y
        in
        f [ 1, 2 ] [ 2, 3 ]



-----


{-| Test: SdkBasics/maxInt
Expected: 4
-}
sdkMaxTestInt : TestContext -> Int
sdkMaxTestInt ctx =
    test ctx <|
        let
            f x y =
                max x y
        in
        f 2 4


{-| Test: SdkBasics/maxFloat
Expected: 4.0
-}
sdkMaxTestFloat : TestContext -> Float
sdkMaxTestFloat ctx =
    test ctx <|
        let
            f x y =
                max x y
        in
        f 2.0 4.0


{-| Test: SdkBasics/maxChar
Expected: 'b'
-}
sdkMaxTestChar : TestContext -> Char
sdkMaxTestChar ctx =
    test ctx <|
        let
            f x y =
                max x y
        in
        f 'a' 'b'


{-| Test: SdkBasics/maxString
Expected: "BB"
-}
sdkMaxTestString : TestContext -> String
sdkMaxTestString ctx =
    test ctx <|
        let
            f x y =
                max x y
        in
        f "AA" "BB"


{-| Test: SdkBasics/maxTuple
Expected: (2, 3)
-}
sdkMaxTestTuple : TestContext -> ( number, number1 )
sdkMaxTestTuple ctx =
    test ctx <|
        let
            f x y =
                max x y
        in
        f ( 1, 2 ) ( 2, 3 )


{-| Test: SdkBasics/maxList
Expected: [2, 3]
-}
sdkMaxTestList : TestContext -> List number
sdkMaxTestList ctx =
    test ctx <|
        let
            f x y =
                max x y
        in
        f [ 1, 2 ] [ 2, 3 ]



-----


{-| Test: SdkBasics/minInt
Expected: 2
-}
sdkMinTestInt : TestContext -> Int
sdkMinTestInt ctx =
    test ctx <|
        let
            f x y =
                min x y
        in
        f 2 4


{-| Test: SdkBasics/minFloat
Expected: 2.0
-}
sdkMinTestFloat : TestContext -> Float
sdkMinTestFloat ctx =
    test ctx <|
        let
            f x y =
                min x y
        in
        f 2.0 4.0


{-| Test: SdkBasics/minChar
Expected: 'a'
-}
sdkMinTestChar : TestContext -> Char
sdkMinTestChar ctx =
    test ctx <|
        let
            f x y =
                min x y
        in
        f 'a' 'b'


{-| Test: SdkBasics/minString
Expected: "AA"
-}
sdkMinTestString : TestContext -> String
sdkMinTestString ctx =
    test ctx <|
        let
            f x y =
                min x y
        in
        f "AA" "BB"


{-| Test: SdkBasics/minTuple
Expected: (1, 2)
-}
sdkMinTestTuple : TestContext -> ( number, number1 )
sdkMinTestTuple ctx =
    test ctx <|
        let
            f x y =
                min x y
        in
        f ( 1, 2 ) ( 2, 3 )


{-| Test: SdkBasics/minList
Expected: [1, 2]
-}
sdkMinTestList : TestContext -> List number
sdkMinTestList ctx =
    test ctx <|
        let
            f x y =
                min x y
        in
        f [ 1, 2 ] [ 2, 3 ]



-----


{-| Test: SdkBasics/orderToString
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


sdkLessThanTest : comparable -> comparable -> Bool
sdkLessThanTest a b =
    a < b


{-| Test: SdkBasics/greaterThan
-}
sdkGreaterThanTest : comparable -> comparable -> Bool
sdkGreaterThanTest a b =
    a > b


{-| Test: SdkBasics/lessThanOrEqual
-}
sdkLessThanOrEqualTest : comparable -> comparable -> Bool
sdkLessThanOrEqualTest a b =
    a <= b


{-| Test: SdkBasics/greaterThanOrEqual
-}
sdkGreaterThanOrEqualTest : comparable -> comparable -> Bool
sdkGreaterThanOrEqualTest a b =
    a >= b


{-| Test: SdkBasics/max
-}
sdkMaxTest : comparable -> comparable -> comparable
sdkMaxTest a b =
    max a b


{-| Test: SdkBasics/min
-}
sdkMinTest : comparable -> comparable -> comparable
sdkMinTest a b =
    min a b


{-| Test: SdkBasics/compare
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


{-| Test: SdkBasics/xor
Expected: True
-}
sdkXorTest : TestContext -> Bool
sdkXorTest ctx =
    test ctx <|
        let
            f x y =
                xor x y
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
sdkAppendListTest : TestContext -> List number
sdkAppendListTest ctx =
    test ctx <|
        let
            f x y =
                List.append x y
        in
        f [ 1, 2 ] [ 3, 4 ]



-----


{-| Test: SdkBasics/identityInt
Expected: 4
-}
sdkIdentityTestInt : TestContext -> Int
sdkIdentityTestInt ctx =
    test ctx <|
        let
            f x =
                identity x
        in
        f 4


{-| Test: SdkBasics/identityFloat
Expected: 4.0
-}
sdkIdentityTestFloat : TestContext -> Float
sdkIdentityTestFloat ctx =
    test ctx <|
        let
            f x =
                identity x
        in
        f 4.0


{-| Test: SdkBasics/identityChar
Expected: 'b'
-}
sdkIdentityTestChar : TestContext -> Char
sdkIdentityTestChar ctx =
    test ctx <|
        let
            f x =
                identity x
        in
        f 'b'


{-| Test: SdkBasics/identityString
Expected: "BB"
-}
sdkIdentityTestString : TestContext -> String
sdkIdentityTestString ctx =
    test ctx <|
        let
            f x =
                identity x
        in
        f "BB"


{-| Test: SdkBasics/identityTuple
Expected: (2, 3)
-}
sdkIdentityTestTuple : TestContext -> ( number, number1 )
sdkIdentityTestTuple ctx =
    test ctx <|
        let
            f x =
                identity x
        in
        f ( 2, 3 )


{-| Test: SdkBasics/identityList
Expected: [2, 3]
-}
sdkIdentityTestList : TestContext -> List number
sdkIdentityTestList ctx =
    test ctx <|
        let
            f x =
                identity x
        in
        f [ 2, 3 ]



-----


{-| Test: SdkBasics/identityInt
Expected: 4
-}
sdkIdentityTestInt : TestContext -> Int
sdkIdentityTestInt ctx =
    test ctx <|
        let
            f x =
                identity x
        in
        f 4


{-| Test: SdkBasics/identityFloat
Expected: 4.0
-}
sdkIdentityTestFloat : TestContext -> Float
sdkIdentityTestFloat ctx =
    test ctx <|
        let
            f x =
                identity x
        in
        f 4.0


{-| Test: SdkBasics/identityChar
Expected: 'b'
-}
sdkIdentityTestChar : TestContext -> Char
sdkIdentityTestChar ctx =
    test ctx <|
        let
            f x =
                identity x
        in
        f 'b'


{-| Test: SdkBasics/identityString
Expected: "BB"
-}
sdkIdentityTestString : TestContext -> String
sdkIdentityTestString ctx =
    test ctx <|
        let
            f x =
                identity x
        in
        f "BB"


{-| Test: SdkBasics/identityTuple
Expected: (2, 3)
-}
sdkIdentityTestTuple : TestContext -> ( number, number1 )
sdkIdentityTestTuple ctx =
    test ctx <|
        let
            f x =
                identity x
        in
        f ( 2, 3 )


{-| Test: SdkBasics/identityList
Expected: [2, 3]
-}
sdkIdentityTestList : TestContext -> List number
sdkIdentityTestList ctx =
    test ctx <|
        let
            f x =
                identity x
        in
        f [ 2, 3 ]



-----


{-| Test: SdkBasics/alwaysInt
Expected: [0]
-}
sdkAlwaysTestInt : TestContext -> List Int
sdkAlwaysTestInt ctx =
    test ctx <|
        let
            f x =
                List.map (always 0) x
        in
        f [ 4 ]


{-| Test: SdkBasics/alwaysFloat
Expected: [0.0]
-}
sdkAlwaysTestFloat : TestContext -> List Float
sdkAlwaysTestFloat ctx =
    test ctx <|
        let
            f x =
                List.map (always 0.0) x
        in
        f [ 4.0 ]


{-| Test: SdkBasics/alwaysChar
Expected: ['a']
-}
sdkAlwaysTestChar : TestContext -> List Char
sdkAlwaysTestChar ctx =
    test ctx <|
        let
            f x =
                List.map (always 'a') x
        in
        f [ 'b' ]


{-| Test: SdkBasics/alwaysString
Expected: ["A"]
-}
sdkAlwaysTestString : TestContext -> List String
sdkAlwaysTestString ctx =
    test ctx <|
        let
            f x =
                List.map (always "A") x
        in
        f [ "B" ]



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
-}
basicsCeilingTest : Float -> Int
basicsCeilingTest x =
    ceiling x


{-| Test: SdkBasics/basicsFloorTest
-}
basicsFloorTest : Float -> Int
basicsFloorTest x =
    floor x


{-| Test: SdkBasics/basicsIntegerDivideTest
-}
basicsIntegerDivideTest : Int -> Int -> Int
basicsIntegerDivideTest x y =
    integerDivide x y


{-| Test: SdkBasics/basicsAbsTest
-}
basicsAbsTest : Float -> Float
basicsAbsTest x =
    abs x


{-| Test: SdkBasics/basicsAlwaysTest
-}
basicsAlwaysTest : a -> List a
basicsAlwaysTest x =
    let
        f y =
            List.map (always x) y
    in
    f [ 4 ]


{-| Test: SdkBasics/basicsClampTest
-}
basicsClampTest : a -> a -> a -> a
basicsClampTest min max x =
    clamp min max x


{-| Test: SdkBasics/basicsIdentityTest
-}
basicsIdentityTest : a -> a
basicsIdentityTest x =
    x


{-| Test: SdkBasics/basicsPowerTest
-}
basicsPowerTest : a -> a -> a
basicsPowerTest n x =
    power n x


{-| Test: SdkBasics/basicsRemainderByTest
-}
basicsRemainderByTest : Int -> Int -> Int
basicsRemainderByTest x y =
    remainderBy x y


{-| Test: SdkBasics/basicsSqrtTest
-}
basicsSqrtTest : Float -> Float
basicsSqrtTest x =
    sqrt x


{-| Test: SdkBasics/basicsTruncateTest
-}
basicsTruncateTest : Float -> Int
basicsTruncateTest x =
    truncate x


{-| Test: SdkBasics/basicsXorTest
-}
basicsXorTest : Bool -> Bool -> Bool
basicsXorTest x y =
    xor x y
