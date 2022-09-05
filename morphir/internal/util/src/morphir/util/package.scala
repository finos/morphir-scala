package morphir

package object util:
  def unreachable:Nothing = 
    throw UnreachableException  
