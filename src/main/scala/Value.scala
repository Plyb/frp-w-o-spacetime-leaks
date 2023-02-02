package main.scala
import adjs.Store
import adjs.Qualifiers

sealed trait Value
sealed trait Stable extends Value

case class IntV(v: Int) extends Stable
case class LambdaV(param: String, body: Expression, env: Store, qs: Qualifiers) extends Value
case class RecursiveV(name: String, param: String, body: Expression, env: Store, qs: Qualifiers) extends Value
case class StableV(v: Stable) extends Stable
case class TokenV() extends Value
case class ThunkV(body: Expression, env: Store, qs: Qualifiers) extends Value {
  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[ThunkV] && body.equals(obj.asInstanceOf[ThunkV].body)
  }
}
case class StreamV(v: Value, vs: ThunkV) extends Value
case class NullV() extends Value
