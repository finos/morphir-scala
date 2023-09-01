module Morphir.Examples.App.SdkBasicsTests exposing (..)

import Morphir.SDK.Int exposing (Int64)

{-
  Test: SdkBasics/add
  Expected = 3
-}
sdkAddTest: () -> Int
sdkAddTest _ =
    let
        f x y = x + y
    in
        f 1 2

{-
  Test: SdkBasics/add
  Expected = 3
-}
sdkAddTest64: {a: Int64, b: Int64} -> Int64
sdkAddTest64 t =
    let
        f x y = x + y
    in
        f t.a t.b

{-
  Test: SdkBasics/intOverflow
  Expected = 3

  Scala's Int.MaxValue is (2^31) -1 = 2147483647
  Elm's numerical ranges are platform-dependent
  The values below will produce 36893488147419103000 from the JS engine
-}
sdkIntOverflowTest: () -> Int
sdkIntOverflowTest _ =
    let
        f x y = x + y
    in
        f (2 ^ 64) (2 ^ 64)

{-
  Test: SdkBasics/subtract
  Expected = 2
-}
sdkSubtractTest: () -> Int
sdkSubtractTest _ =
    let
        f x y = x - y
    in
        f 4 2

{-
  Test: SdkBasics/subtract
  Expected = 2
-}
sdkSubtractTest64: {a: Int64, b: Int64} -> Int64
sdkSubtractTest64 t =
    let
        f x y = x - y
    in
        f t.a t.b

{-
  Test: SdkBasics/multiply
  Expected = 6
-}
sdkMultiplyTest: () -> Int
sdkMultiplyTest _ =
    let
        f x y = x * y
    in
        f 2 3

{-
  Test: SdkBasics/addFloat
  Expected = 3.0
-}
sdkAddFloatTest: () -> Float
sdkAddFloatTest _ =
    let
        f x y = x + y
    in
        f 1.0 2.0

{-
  Test: SdkBasics/floatOverflow
  Expected = 3

  Scala's Float.MaxValue is 3.4028235E38
  Elm's numerical ranges are platform-dependent
  The values below will produce 6.80564733841877e+38 from the JS engine
-}
sdkFloatOverflowTest: () -> Float
sdkFloatOverflowTest _ =
    let
        f x y = x + y
    in
        f (2.0 ^ 128) (2.0 ^ 128)

{-
  Test: SdkBasics/subtractFloat
  Expected = 2.0
-}
sdkSubtractFloatTest: () -> Float
sdkSubtractFloatTest _ =
    let
        f x y = x - y
    in
        f 4.0 2.0

{-
  Test: SdkBasics/multiplyFloat
  Expected = 6.0
-}
sdkMultiplyFloatTest: () -> Float
sdkMultiplyFloatTest _ =
    let
        f x y = x * y
    in
        f 2.0 3.0

{-
  Test: SdkBasics/divide
  Expected = 2.0
-}
sdkDivideTest: () -> Float
sdkDivideTest _ =
    let
        f x y = x / y
    in
        f 20.0 10.0

{-
  Test: SdkBasics/divideByZero
  Expected = Infinity
-}
sdkDivideByZeroTest: () -> Float
sdkDivideByZeroTest _ =
    let
        f x y = x / y
    in
        f 20.0 0

{-
  Test: SdkBasics/integerDivide
  Expected = 2
-}
sdkIntegerDivideTest: () -> Int
sdkIntegerDivideTest _ =
    let
        f x y = x // y
    in
        f 20 10

{-
  Test: SdkBasics/toFloat
  Expected = 2.0
-}
toFloatTest: () -> Float
toFloatTest _ =
    let
        f x = toFloat x
    in f 2

{-
  Test: SdkBasics/power
  Expected = 16
-}
sdkPowerTest: () -> Int
sdkPowerTest _ =
    let
        f x y = x ^ y
    in
        f 4 2

{-
  Test: SdkBasics/powerFloat
  Expected = 16
-}
sdkPowerFloatTest: () -> Float
sdkPowerFloatTest _ =
    let
        f x y = x ^ y
    in
        f 4 2

{-
  Test: SdkBasics/round
  Expected = 4
-}
sdkRoundTest: () -> Int
sdkRoundTest _ =
    let
        f x = round x
    in
        f 4.25

{-
  Test: SdkBasics/round2
  Expected = 4
-}
sdkRoundTest2: () -> Int
sdkRoundTest2 _ =
    let
        f x = round x
    in
        f 4

{-
  Test: SdkBasics/floor
  Expected = 5
-}
sdkFloorTest: () -> Int
sdkFloorTest _ =
    let
        f x = floor x
    in
        f 5.75

{-
  Test: SdkBasics/floor2
  Expected = 5
-}
sdkFloorTest2: () -> Int
sdkFloorTest2 _ =
    let
        f x = floor x
    in
        f 5

{-
  Test: SdkBasics/ceiling
  Expected = 8
-}
sdkCeilingTest: () -> Int
sdkCeilingTest _ =
    let
        f x = ceiling x
    in
        f 7.25

{-
  Test: SdkBasics/ceiling2
  Expected = 8
-}
sdkCeilingTest2: () -> Int
sdkCeilingTest2 _ =
    let
        f x = ceiling x
    in
        f 8

{-
  Test: SdkBasics/truncate
  Expected = 5
-}
sdkTruncateTest: () -> Int
sdkTruncateTest _ =
    let
        f x = truncate x
    in
        f 5.25

{-
  Test: SdkBasics/truncate2
  Expected = 5
-}
sdkTruncateTest2: () -> Int
sdkTruncateTest2 _ =
    let
        f x = truncate x
    in
        f 5

{-
  Test: SdkBasics/modBy
  Expected = 2
-}
sdkModByTest: () -> Int
sdkModByTest _ =
    let
        f x y = modBy x y
    in
        f 3 20

{-
  Test: SdkBasics/remainderBy
  Expected = 2
-}
sdkRemainderByTest: () -> Int
sdkRemainderByTest _ =
    let
        f x y = remainderBy x y
    in
        f 3 20

{-
  Test: SdkBasics/negate
  Expected = -3
-}
sdkNegateTest: () -> Int
sdkNegateTest _ =
    let
        f x = negate x
    in
        f 3

{-
  Test: SdkBasics/negate2
  Expected = 3
-}
sdkNegateTest2: () -> Int
sdkNegateTest2 _ =
    let
        f x = negate x
    in
        f -3

{-
  Test: SdkBasics/abs
  Expected = 3
-}
sdkAbsTest: () -> Int
sdkAbsTest _ =
    let
        f x = abs x
    in
        f -3

{-
  Test: SdkBasics/abs2
  Expected = 3
-}
sdkAbsTest2: () -> Int
sdkAbsTest2 _ =
    let
        f x = abs x
    in
        f 3

{-
  Test: SdkBasics/clamp
  Expected = 100
-}
sdkClampTest: () -> Int
sdkClampTest _ =
    let
        f x y z = clamp x y z
    in
        f 100 200 50

{-
  Test: SdkBasics/clamp2
  Expected = 100
-}
sdkClampTest2: () -> Int
sdkClampTest2 _ =
    let
        f x y z = clamp x y z
    in
        f 100 200 100

{-
  Test: SdkBasics/clamp3
  Expected = 200
-}
sdkClampTest3: () -> Int
sdkClampTest3 _ =
    let
        f x y z = clamp x y z
    in
        f 100 200 201

{-
  Test: SdkBasics/isNan
  Expected = True
-}
sdkIsNaNTest: () -> Bool
sdkIsNaNTest _ =
    let
        f x = isNaN x
    in
        f (0/0)

{-
  Test: SdkBasics/isInfinite
  Expected = True
-}
sdkIsInfiniteTest: () -> Bool
sdkIsInfiniteTest _ =
    let
        f x = isInfinite x
    in
        f (1/0)

{-
  Test: SdkBasics/sqrt
  Expected = 4
-}
sdkSqrtTest: () -> Float
sdkSqrtTest _ =
    let
        f x = sqrt x
    in
        f 16

{-
  Test: SdkBasics/logBase
  Expected: 2
-}
sdkLogBaseTest: () -> Float
sdkLogBaseTest _ =
    let
        f x y = logBase x y
    in
        f 10 100

{-
  Test: SdkBasics/logBase2
  Expected: 8
-}
sdkLogBaseTest2: () -> Float
sdkLogBaseTest2 _ =
    let
        f x y = logBase x y
    in
        f 2 256

{-
  Test: SdkBasics/eulersNumber
  Expected: 2.718281828459045
-}
sdkEulersNumberTest: () -> Float
sdkEulersNumberTest _ =
    let
        f = e
    in
        f

{-
  Test: SdkBasics/pi
  Expected: 3.141592653589793
-}
sdkPiTest: () -> Float
sdkPiTest _ =
    let
        f = pi
    in
        f

{-
  Test: SdkBasics/cos
  Expected: 0.5000000000000001
-}
sdkCosTest: () -> Float
sdkCosTest _ =
    let
        f x = cos x
    in
        f (degrees 60)

{-
  Test: SdkBasics/sin
  Expected: 0.8660254037844386
-}
sdkSinTest: () -> Float
sdkSinTest _ =
    let
        f x = sin x
    in
        f (degrees 60)

{-
  Test: SdkBasics/tan
  Expected: 0.9999999999999999
-}
sdkTanTest: () -> Float
sdkTanTest _ =
    let
        f x = tan x
    in
        f (degrees 45)

{-
  Test: SdkBasics/acos
  Expected: 1.0471975511965979
-}
sdkACosTest: () -> Float
sdkACosTest _ =
    let
        f x = acos x
    in
        f (1/2)

{-
  Test: SdkBasics/asin
  Expected: 0.5235987755982989
-}
sdkASinTest: () -> Float
sdkASinTest _ =
    let
        f x = asin x
    in
        f (1/2)

{-
  Test: SdkBasics/atan
  Expected: 0.7853981633974483
-}
sdkATanTest: () -> Float
sdkATanTest _ =
    let
        f x = atan x
    in
        f 1

{-
  Test: SdkBasics/atan2
  Expected: 0.7853981633974483
-}
sdkATan2Test: () -> Float
sdkATan2Test _ =
    let
        f x y = atan2 x y
    in
        f 1 1

{-
  Test: SdkBasics/degrees
  Expected: 3.141592653589793
-}
sdkDegreesTest: () -> Float
sdkDegreesTest _ =
    let
        f x = degrees x
    in
        f 180

{-
  Test: SdkBasics/radians
  Expected: 3.141592653589793
-}
sdkRadiansTest: () -> Float
sdkRadiansTest _ =
    let
        f x = radians x
    in
        f pi

{-
  Test: SdkBasics/turns
  Expected: 3.141592653589793
-}
sdkTurnsTest: () -> Float
sdkTurnsTest _ =
    let
        f x = turns x
    in
        f (1/2)

{-
  Test: SdkBasics/toPolar
  Expected: (5,0.9272952180016122)
-}
sdkToPolarTest: () -> (Float, Float)
sdkToPolarTest _ =
    let
        f x = toPolar x
    in
        f (3, 4)

{-
  Test: SdkBasics/fromPolar
  Expected: ~(1, 1)
-}
sdkFromPolarTest: () -> (Float, Float)
sdkFromPolarTest _ =
    let
        f x = fromPolar x
    in
        f (sqrt 2, degrees 45)

-----

{-
  Test: SdkBasics/equal
  Expected: True
-}
sdkEqualTest: () -> Bool
sdkEqualTest _ =
    let
        f x y = x == y
    in
        f 2 2

{-
  Test: SdkBasics/equal2
  Expected: True
-}
sdkEqualTest2: () -> Bool
sdkEqualTest2 _ =
    let
        f x y = x == y
    in
        f 2.0 2.0

{-
  Test: SdkBasics/equal3
  Expected: True
-}
sdkEqualTest3: () -> Bool
sdkEqualTest3 _ =
    let
        f x y = x == y
    in
        f True True

{-
  Test: SdkBasics/equal4
  Expected: True
-}
sdkEqualTest4: () -> Bool
sdkEqualTest4 _ =
    let
        f x y = x == y
    in
        f 'a' 'a'

{-
  Test: SdkBasics/equal5
  Expected: True
-}
sdkEqualTest5: () -> Bool
sdkEqualTest5 _ =
    let
        f x y = x == y
    in
        f "yo" "yo"

{-
  Test: SdkBasics/equal6
  Expected: True
-}
sdkEqualTest6: () -> Bool
sdkEqualTest6 _ =
    let
        f x y = x == y
    in
        f [1, 2] [1, 2]

{-
  Test: SdkBasics/equal7
  Expected: True
-}
sdkEqualTest7: () -> Bool
sdkEqualTest7 _ =
    let
        f x y = x == y
    in
        f (1, 2, 3) (1, 2, 3)

-----

{-
  Test: SdkBasics/notEqual
  Expected: True
-}
sdkNotEqualTest: () -> Bool
sdkNotEqualTest _ =
    let
        f x y = x /= y
    in
        f 1 2

{-
  Test: SdkBasics/notEqual2
  Expected: True
-}
sdkNotEqualTest2: () -> Bool
sdkNotEqualTest2 _ =
    let
        f x y = x /= y
    in
        f 1.0 2.0

{-
  Test: SdkBasics/notEqual3
  Expected: True
-}
sdkNotEqualTest3: () -> Bool
sdkNotEqualTest3 _ =
    let
        f x y = x /= y
    in
        f False True

{-
  Test: SdkBasics/notEqual4
  Expected: True
-}
sdkNotEqualTest4: () -> Bool
sdkNotEqualTest4 _ =
    let
        f x y = x /= y
    in
        f 'a' 'b'

{-
  Test: SdkBasics/notEqual5
  Expected: True
-}
sdkNotEqualTest5: () -> Bool
sdkNotEqualTest5 _ =
    let
        f x y = x /= y
    in
        f "yo" "yoo"

{-
  Test: SdkBasics/notEqual6
  Expected: True
-}
sdkNotEqualTest6: () -> Bool
sdkNotEqualTest6 _ =
    let
        f x y = x /= y
    in
        f [1, 2] [1, 2, 3]

{-
  Test: SdkBasics/notEqual7
  Expected: True
-}
sdkNotEqualTest7: () -> Bool
sdkNotEqualTest7 _ =
    let
        f x y = x /= y
    in
        f (1, 2, 3) (1, 2, 4)

-----

{-
  Test: SdkBasics/lessThanInt
  Expected: True
-}
sdkLessThanTestIntTrue: () -> Bool
sdkLessThanTestIntTrue _ =
    let f x y = x < y in f 2 4

{-
  Test: SdkBasics/lessThanInt
  Expected: False
-}
sdkLessThanTestIntFalse: () -> Bool
sdkLessThanTestIntFalse _ =
    let f x y = x < y in f 5 4


{-
  Test: SdkBasics/greaterThanInt
  Expected: True
-}
sdkGreaterThanTestIntTrue: () -> Bool
sdkGreaterThanTestIntTrue _ =
    let f x y = x > y in f 4 2

{-
  Test: SdkBasics/greaterThanInt
  Expected: False
-}
sdkGreaterThanTestIntFalse: () -> Bool
sdkGreaterThanTestIntFalse _ =
    let f x y = x > y in f 1 2

{-
  Test: SdkBasics/greaterThanEqualsInt (1)
  Expected: True
-}
sdkGreaterThanOrEqualTestIntTrue1: () -> Bool
sdkGreaterThanOrEqualTestIntTrue1 _ =
    let f x y = x >= y in f 4 2

{-
  Test: SdkBasics/greaterThanEqualsInt (2)
  Expected: True
-}
sdkGreaterThanOrEqualTestIntTrue2: () -> Bool
sdkGreaterThanOrEqualTestIntTrue2 _ =
    let f x y = x >= y in f 2 2

{-
  Test: SdkBasics/greaterThanEqualsInt
  Expected: False
-}
sdkGreaterThanOrEqualTestIntFalse: () -> Bool
sdkGreaterThanOrEqualTestIntFalse _ =
    let f x y = x >= y in f 1 2

{-
  Test: SdkBasics/lessThanEqualsInt (1)
  Expected: True
-}
sdkLessThanOrEqualTestIntTrue1: () -> Bool
sdkLessThanOrEqualTestIntTrue1 _ =
    let f x y = x <= y in f 2 4

{-
  Test: SdkBasics/lessThanEqualsInt (2)
  Expected: True
-}
sdkLessThanOrEqualTestIntTrue2: () -> Bool
sdkLessThanOrEqualTestIntTrue2 _ =
    let f x y = x <= y in f 2 2

{-
  Test: SdkBasics/lessThanEqualsInt
  Expected: False
-}
sdkLessThanOrEqualTestIntFalse: () -> Bool
sdkLessThanOrEqualTestIntFalse _ =
    let f x y = x <= y in f 2 1


{-
  Test: SdkBasics/lessThanFloat
  Expected: True
-}
sdkLessThanTestFloat: () -> Bool
sdkLessThanTestFloat _ =
    let
        f x y = x < y
    in
        f 2.0 4.0

{-
  Test: SdkBasics/lessThanChar
  Expected: True
-}
sdkLessThanTestChar: () -> Bool
sdkLessThanTestChar _ =
    let
        f x y = x < y
    in
        f 'a' 'b'

{-
  Test: SdkBasics/lessThanString
  Expected: True
-}
sdkLessThanTestString: () -> Bool
sdkLessThanTestString _ =
    let
        f x y = x < y
    in
        f "AA" "BB"

{-
  Test: SdkBasics/lessThanTuple
  Expected: True
-}
sdkLessThanTestTuple: () -> Bool
sdkLessThanTestTuple _ =
    let
        f x y = x < y
    in
        f (1, 2) (2, 3)

{-
  Test: SdkBasics/lessThanList
  Expected: True
-}
sdkLessThanTestList: () -> Bool
sdkLessThanTestList _ =
    let
        f x y = x < y
    in
        f [1, 2] [2, 3]

-----

{-
  Test: SdkBasics/lessThanOrEqualInt
  Expected: True
-}
sdkLessThanOrEqualTestInt: () -> Bool
sdkLessThanOrEqualTestInt _ =
    let
        f x y = x <= y
    in
        f 2 4

{-
  Test: SdkBasics/lessThanOrEqualFloat
  Expected: True
-}
sdkLessThanOrEqualTestFloat: () -> Bool
sdkLessThanOrEqualTestFloat _ =
    let
        f x y = x <= y
    in
        f 2.0 4.0

{-
  Test: SdkBasics/lessThanOrEqualChar
  Expected: True
-}
sdkLessThanOrEqualTestChar: () -> Bool
sdkLessThanOrEqualTestChar _ =
    let
        f x y = x <= y
    in
        f 'a' 'b'

{-
  Test: SdkBasics/lessThanOrEqualString
  Expected: True
-}
sdkLessThanOrEqualTestString: () -> Bool
sdkLessThanOrEqualTestString _ =
    let
        f x y = x <= y
    in
        f "AA" "BB"

{-
  Test: SdkBasics/lessThanOrEqualTuple
  Expected: True
-}
sdkLessThanOrEqualTestTuple: () -> Bool
sdkLessThanOrEqualTestTuple _ =
    let
        f x y = x <= y
    in
        f (1, 2) (2, 3)

{-
  Test: SdkBasics/lessThanOrEqualList
  Expected: True
-}
sdkLessThanOrEqualTestList: () -> Bool
sdkLessThanOrEqualTestList _ =
    let
        f x y = x <= y
    in
        f [1, 2] [2, 3]

-----

{-
  Test: SdkBasics/greaterThanInt
  Expected: False
-}
sdkGreaterThanTestInt: () -> Bool
sdkGreaterThanTestInt _ =
    let
        f x y = x > y
    in
        f 2 4

{-
  Test: SdkBasics/greaterThanFloat
  Expected: False
-}
sdkGreaterThanTestFloat: () -> Bool
sdkGreaterThanTestFloat _ =
    let
        f x y = x > y
    in
        f 2.0 4.0

{-
  Test: SdkBasics/greaterThanChar
  Expected: False
-}
sdkGreaterThanTestChar: () -> Bool
sdkGreaterThanTestChar _ =
    let
        f x y = x > y
    in
        f 'a' 'b'

{-
  Test: SdkBasics/greaterThanString
  Expected: False
-}
sdkGreaterThanTestString: () -> Bool
sdkGreaterThanTestString _ =
    let
        f x y = x > y
    in
        f "AA" "BB"

{-
  Test: SdkBasics/greaterThanTuple
  Expected: False
-}
sdkGreaterThanTestTuple: () -> Bool
sdkGreaterThanTestTuple _ =
    let
        f x y = x > y
    in
        f (1, 2) (2, 3)

{-
  Test: SdkBasics/greaterThanList
  Expected: False
-}
sdkGreaterThanTestList: () -> Bool
sdkGreaterThanTestList _ =
    let
        f x y = x > y
    in
        f [1, 2] [2, 3]

-----

{-
  Test: SdkBasics/greaterThanOrEqualInt
  Expected: False
-}
sdkGreaterThanOrEqualTestInt: () -> Bool
sdkGreaterThanOrEqualTestInt _ =
    let
        f x y = x >= y
    in
        f 2 4

{-
  Test: SdkBasics/greaterThanOrEqualFloat
  Expected: False
-}
sdkGreaterThanOrEqualTestFloat: () -> Bool
sdkGreaterThanOrEqualTestFloat _ =
    let
        f x y = x >= y
    in
        f 2.0 4.0

{-
  Test: SdkBasics/greaterThanOrEqualChar
  Expected: False
-}
sdkGreaterThanOrEqualTestChar: () -> Bool
sdkGreaterThanOrEqualTestChar _ =
    let
        f x y = x >= y
    in
        f 'a' 'b'

{-
  Test: SdkBasics/greaterThanOrEqualString
  Expected: False
-}
sdkGreaterThanOrEqualTestString: () -> Bool
sdkGreaterThanOrEqualTestString _ =
    let
        f x y = x >= y
    in
        f "AA" "BB"

{-
  Test: SdkBasics/greaterThanOrEqualTuple
  Expected: False
-}
sdkGreaterThanOrEqualTestTuple: () -> Bool
sdkGreaterThanOrEqualTestTuple _ =
    let
        f x y = x >= y
    in
        f (1, 2) (2, 3)

{-
  Test: SdkBasics/greaterThanOrEqualList
  Expected: False
-}
sdkGreaterThanOrEqualTestList: () -> Bool
sdkGreaterThanOrEqualTestList _ =
    let
        f x y = x >= y
    in
        f [1, 2] [2, 3]

-----

{-
  Test: SdkBasics/maxInt
  Expected: 4
-}
sdkMaxTestInt: () -> Int
sdkMaxTestInt _ =
    let
        f x y = max x y
    in
        f 2 4

{-
  Test: SdkBasics/maxFloat
  Expected: 4.0
-}
sdkMaxTestFloat: () -> Float
sdkMaxTestFloat _ =
    let
        f x y = max x y
    in
        f 2.0 4.0

{-
  Test: SdkBasics/maxChar
  Expected: 'b'
-}
sdkMaxTestChar: () -> Char
sdkMaxTestChar _ =
    let
        f x y = max x y
    in
        f 'a' 'b'

{-
  Test: SdkBasics/maxString
  Expected: "BB"
-}
sdkMaxTestString: () -> String
sdkMaxTestString _ =
    let
        f x y = max x y
    in
        f "AA" "BB"

{-
  Test: SdkBasics/maxTuple
  Expected: (2, 3)
-}
sdkMaxTestTuple: () -> ( number, number1 )
sdkMaxTestTuple _ =
    let
        f x y = max x y
    in
        f (1, 2) (2, 3)

{-
  Test: SdkBasics/maxList
  Expected: [2, 3]
-}
sdkMaxTestList: () -> List number
sdkMaxTestList _ =
    let
        f x y = max x y
    in
        f [1, 2] [2, 3]

-----

{-
  Test: SdkBasics/minInt
  Expected: 2
-}
sdkMinTestInt: () -> Int
sdkMinTestInt _ =
    let
        f x y = min x y
    in
        f 2 4

{-
  Test: SdkBasics/minFloat
  Expected: 2.0
-}
sdkMinTestFloat: () -> Float
sdkMinTestFloat _ =
    let
        f x y = min x y
    in
        f 2.0 4.0

{-
  Test: SdkBasics/minChar
  Expected: 'a'
-}
sdkMinTestChar: () -> Char
sdkMinTestChar _ =
    let
        f x y = min x y
    in
        f 'a' 'b'

{-
  Test: SdkBasics/minString
  Expected: "AA"
-}
sdkMinTestString: () -> String
sdkMinTestString _ =
    let
        f x y = min x y
    in
        f "AA" "BB"

{-
  Test: SdkBasics/minTuple
  Expected: (1, 2)
-}
sdkMinTestTuple: () -> ( number, number1 )
sdkMinTestTuple _ =
    let
        f x y = min x y
    in
        f (1, 2) (2, 3)

{-
  Test: SdkBasics/minList
  Expected: [1, 2]
-}
sdkMinTestList: () -> List number
sdkMinTestList _ =
    let
        f x y = min x y
    in
        f [1, 2] [2, 3]

-----

{-
  Test: SdkBasics/not
  Expected: False
-}
sdkNotTest: () -> Bool
sdkNotTest _ =
    let
        f x = not x
    in
        f True

{-
  Test: SdkBasics/and
  Expected: False
-}
sdkAndTest: () -> Bool
sdkAndTest _ =
    let
        f x y = x && y
    in
        f True False

{-
  Test: SdkBasics/or
  Expected: True
-}
sdkOrTest: () -> Bool
sdkOrTest _ =
    let
        f x y = x || y
    in
        f True False

{-
  Test: SdkBasics/xor
  Expected: True
-}
sdkXorTest: () -> Bool
sdkXorTest _ =
    let
        f x y = xor x y
    in
        f True False

-----

{-
  Test: SdkBasics/appendString
  Expected: "aa-bb"
-}
sdkAppendStringTest: () -> String
sdkAppendStringTest _ =
    let
        f x y = String.append x y
    in
        f "aa-" "bb"

{-
  Test: SdkBasics/appendList
  Expected: [1, 2, 3, 4]
-}
sdkAppendListTest: () -> List number
sdkAppendListTest _ =
    let
        f x y = List.append x y
    in
        f [1, 2] [3, 4]

-----


{-
  Test: SdkBasics/identityInt
  Expected: 4
-}
sdkIdentityTestInt: () -> Int
sdkIdentityTestInt _ =
    let
        f x = identity x
    in
        f 4

{-
  Test: SdkBasics/identityFloat
  Expected: 4.0
-}
sdkIdentityTestFloat: () -> Float
sdkIdentityTestFloat _ =
    let
        f x = identity x
    in
        f 4.0

{-
  Test: SdkBasics/identityChar
  Expected: 'b'
-}
sdkIdentityTestChar: () -> Char
sdkIdentityTestChar _ =
    let
        f x = identity x
    in
        f 'b'

{-
  Test: SdkBasics/identityString
  Expected: "BB"
-}
sdkIdentityTestString: () -> String
sdkIdentityTestString _ =
    let
        f x = identity x
    in
        f "BB"

{-
  Test: SdkBasics/identityTuple
  Expected: (2, 3)
-}
sdkIdentityTestTuple: () -> ( number, number1 )
sdkIdentityTestTuple _ =
    let
        f x = identity x
    in
        f (2, 3)

{-
  Test: SdkBasics/identityList
  Expected: [2, 3]
-}
sdkIdentityTestList: () -> List number
sdkIdentityTestList _ =
    let
        f x = identity x
    in
        f [2, 3]

-----

{-
  Test: SdkBasics/identityInt
  Expected: 4
-}
sdkIdentityTestInt: () -> Int
sdkIdentityTestInt _ =
    let
        f x = identity x
    in
        f 4

{-
  Test: SdkBasics/identityFloat
  Expected: 4.0
-}
sdkIdentityTestFloat: () -> Float
sdkIdentityTestFloat _ =
    let
        f x = identity x
    in
        f 4.0

{-
  Test: SdkBasics/identityChar
  Expected: 'b'
-}
sdkIdentityTestChar: () -> Char
sdkIdentityTestChar _ =
    let
        f x = identity x
    in
        f 'b'

{-
  Test: SdkBasics/identityString
  Expected: "BB"
-}
sdkIdentityTestString: () -> String
sdkIdentityTestString _ =
    let
        f x = identity x
    in
        f "BB"

{-
  Test: SdkBasics/identityTuple
  Expected: (2, 3)
-}
sdkIdentityTestTuple: () -> ( number, number1 )
sdkIdentityTestTuple _ =
    let
        f x = identity x
    in
        f (2, 3)

{-
  Test: SdkBasics/identityList
  Expected: [2, 3]
-}
sdkIdentityTestList: () -> List number
sdkIdentityTestList _ =
    let
        f x = identity x
    in
        f [2, 3]

-----

{-
  Test: SdkBasics/alwaysInt
  Expected: [0]
-}
sdkAlwaysTestInt: () -> List Int
sdkAlwaysTestInt _ =
    let
        f x = List.map (always 0) x
    in
        f [4]

{-
  Test: SdkBasics/alwaysFloat
  Expected: [0.0]
-}
sdkAlwaysTestFloat: () -> List Float
sdkAlwaysTestFloat _ =
    let
        f x = List.map (always 0.0) x
    in
        f [4.0]

{-
  Test: SdkBasics/alwaysChar
  Expected: ['a']
-}
sdkAlwaysTestChar: () -> List Char
sdkAlwaysTestChar _ =
    let
        f x = List.map (always 'a') x
    in
        f ['b']

{-
  Test: SdkBasics/alwaysString
  Expected: ["A"]
-}
sdkAlwaysTestString: () -> List String
sdkAlwaysTestString _ =
    let
        f x = List.map (always "A") x
    in
        f ["B"]

-----

{-
  Test: SdkBasics/composeLeft
  Expected: False
-}
sdkComposeLeftTest: () -> Bool
sdkComposeLeftTest _ =
    let
        f x y z = x << y << z
        g = f not not not
    in
        g True

{-
  Test: SdkBasics/composeRight
  Expected: True
-}
sdkComposeRightTest: () -> Bool
sdkComposeRightTest _ =
    let
        f x y z = x >> y >> z
        g = f not not not
    in
        g False
