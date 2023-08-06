package org.finos.morphir

package object codecs {

  // NOTE: Wrote this way so you can also say Out Encoder Data i.e. JSON Encoder Person
  type Encoder[Out, Data] = Writer[Any, Data, Out]
}
