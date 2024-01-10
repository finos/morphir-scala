package org.finos.morphir

package object runtime {

  // The strange implicit condition below ensures that these methods are only
  // available if directly using RTValue. Once something is converted into a
  // specific type of RTValue however, just use the `unwrap` method in order
  // to get the value of RTValue-thing.
  // Things like coerceList for example should Never be available on a
  // RTValue.Boolean. Only the `.unwrap` function which will unwrap
  // the aforementioned value into a boolean.
  implicit class ResultOp[T <: RTValue](self: T)(implicit has: T =:= RTValue) {
    def coerceString     = RTValue.coerceString(self)
    def coerceInt        = RTValue.coerceInt(self)
    def coerceBoolean    = RTValue.coerceBoolean(self)
    def coerceDouble     = RTValue.coerceDouble(self)
    def coerceFloat      = RTValue.coerceFloat(self)
    def coerceDecimal    = RTValue.coerceDecimal(self)
    def coercePrimitive  = RTValue.coercePrimitive(self)
    def coerceNumeric    = RTValue.coerceNumeric(self)
    def coerceComparable = RTValue.coerceComparable(self)
    def coerceList       = RTValue.coerceList(self)
    def coerceSet        = RTValue.coerceSet(self)
    def coerceTuple      = RTValue.coerceTuple(self)
    def coerceMap        = RTValue.coerceMap(self)
  }

  type ??? = Nothing
}
