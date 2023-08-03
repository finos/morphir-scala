package org.finos.morphir.universe
package sdk

package object types {
  type MInteger = Basics.Integer
  val MInteger: Basics.Integer.type = Basics.Integer

  type MFloat = Basics.Float
  val MFloat: Basics.Float.type = Basics.Float

  type Int8  = sdk.Int.Int8
  type Int16 = sdk.Int.Int16
  type Int32 = sdk.Int.Int32
  type Int64 = sdk.Int.Int64

}
