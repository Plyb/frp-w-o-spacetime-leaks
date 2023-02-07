package main.scala
import adjs.Store
import adjs.Qualifiers

sealed trait Value

case class IntV(v: Int) extends Value
case class LambdaV(param: String, body: Expression, env: Store, qs: Qualifiers) extends Value
case class StableV(v: Value) extends Value
case class TokenV() extends Value
case class ThunkV(body: Expression, env: Store, qs: Qualifiers) extends Value {
  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[ThunkV] && body.equals(obj.asInstanceOf[ThunkV].body)
  }
}
case class StreamV(v: Value, vs: ThunkV) extends Value
case class PairV(fst: Value, snd: Value) extends Value
case class InlV(v: Value) extends Value
case class InrV(v: Value) extends Value
case class IntoV(v: Value) extends Value
case class NullV() extends Value
