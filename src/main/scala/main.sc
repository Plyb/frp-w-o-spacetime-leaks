import scala.collection.immutable.Map

sealed trait Expression
case class IntE(v: Int) extends Expression
case class VarE(name: String) extends Expression
case class PromoteE(exp: Expression) extends Expression
case class ConsE(lhs: Expression, rhs: Expression) extends Expression
case class DelayE(token: Expression, exp: Expression) extends Expression
case class LetConsInE(lhs: VarE, rhs: VarE, exp: Expression, body: Expression) extends Expression
case class LetStableInE(name: VarE, exp: Expression, body: Expression) extends Expression
case class LetDelayInE(name: VarE, exp: Expression, body: Expression) extends  Expression
case class LambdaE(name: VarE, body: Expression) extends Expression
case class ApplicationE(lhs: Expression, rhs: Expression) extends Expression

sealed trait Value

type Store = Map[String, Value]

case class IntV(v: Int) extends Value
case class LambdaV(name: String, body: Expression, env: Store) extends Value
case class NullV() extends Value


val yCombinator = LambdaE(VarE("f"),
  ApplicationE(
    LambdaE(VarE("x"), ApplicationE(VarE("f"), ApplicationE(VarE("x"), VarE("x")))),
    LambdaE(VarE("x"), ApplicationE(VarE("f"), ApplicationE(VarE("x"), VarE("x"))))));

def runWithStore(exp: Expression, store: Store): Value = {
  exp match {
    case IntE(v) => IntV(v)
    case VarE(name) => store.getOrElse(name, NullV())
    case LambdaE(VarE(name), body) => LambdaV(name, body, store)
    case ApplicationE(lhs, rhs) => {
      val lhsv = runWithStore(lhs, store)
      lhsv match {
        case LambdaV(name, body, env) => {
          val rhsv = runWithStore(rhs, store)
          runWithStore(body, env + (name -> rhsv))
        }
        case _ => throw new Exception("Non-function lhs of application")
      }
    }
  }
}

def test(exp: Expression, v: Value, store: Store = Map.empty): Unit =
  assert(runWithStore(exp, Map.empty) == v, store)

test(IntE(1), IntV(1))
test(LambdaE(VarE("x"), VarE("x")), LambdaV("x", VarE("x"), Map.empty))
test(ApplicationE(LambdaE(VarE("x"), VarE("x")), IntE(42)), IntV(42))
runWithStore(ApplicationE(yCombinator, LambdaE(VarE("f"), VarE("f"))), Map.empty)
