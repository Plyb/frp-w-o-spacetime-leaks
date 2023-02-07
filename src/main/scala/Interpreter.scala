package main.scala
import adjs.Store
import adjs.Qualifiers

object Interpreter {
  private def replace(source: Expression)(implicit toReplace: String, replacement: Expression): Expression = {
    source match {
      case VarE(name) if name == toReplace => replacement
      case PromoteE(exp) => PromoteE(replace(exp))
      case ConsE(lhs, rhs) => ConsE(replace(lhs), replace(rhs))
      case DelayE(token, exp) => DelayE(replace(token), replace(exp))
      case LetConsInE(lhs, rhs, exp, body) => LetConsInE(lhs, rhs, replace(exp),
        if (toReplace == lhs || toReplace == rhs) body else  replace(body))
      case LetStableInE(param, exp, body) => LetStableInE(param, replace(exp),
        if (param == toReplace) body else replace(body))
      case LetDelayInE(param, exp, body) => LetDelayInE(param, replace(exp),
        if (param == toReplace) body else replace(body))
      case LambdaE(param, body) => LambdaE(param, if (param == toReplace) body else replace(body))
      case AppE(lhs, rhs) => AppE(replace(lhs), replace(rhs))
      case PairE(fst, snd) => PairE(replace(fst), replace(snd))
      case FstE(exp) => FstE(replace(exp))
      case SndE(exp) => SndE(replace(exp))
      case InlE(exp) => InlE(replace(exp))
      case InrE(exp) => InrE(replace(exp))
      case CaseE(exp, inlParam, inlBody, inrParam, inrBody) =>
        CaseE(replace(exp), inlParam, replace(inlBody), inrParam, replace(inrBody))
      case IntoE(exp) => IntoE(replace(exp))
      case OutE(exp) => OutE(replace(exp))
      case _ => source
    }
  }

  private def addToQs(qs: Qualifiers, id: String, v: Value): Qualifiers = {
    qs + (id -> (if (Stability.isStable(v)) StableQ() else NowQ()))
  }

  private def eval(exp: Expression)(implicit store: Store, qs: Qualifiers): Value = {
    exp match {
      case IntE(v) => IntV(v)
      case VarE(name) => store.getOrElse(name, throw new Exception("Tried to dereference a null value: " + name))
      case LambdaE(param, body) => LambdaV(param, body, store, qs)
      case AppE(lhs, rhs) => {
        val lhsv = eval(lhs)
        lhsv match {
          case LambdaV(param, body, env, lambdaQs) => {
            val rhsv = eval(rhs)
            eval(body)(env + (param -> rhsv), addToQs(lambdaQs, param, rhsv))
          }
          case _ => throw new Exception("Non-function lhs of application: " + lhs.toString)
        }
      }
      case PromoteE(exp) => {
        val v = eval(exp)
        if (!Stability.isStable(v)) {
          throw new Exception("Tried to promote non-stable value")
        }
        StableV(v)
      }
      case LetStableInE(param, exp, body) => {
        val expV = eval(exp)
        expV match {
          case StableV(v) => eval(body)(store + (param -> v), qs + (param -> StableQ()))
          case _ => throw new Exception("Tried to stable-deconstruct a non-square type")
        }
      }
      case DelayE(tokenE, exp) => {
        val tokenV = eval(tokenE)
        tokenV match {
          case TokenV() => ThunkV(exp, store, qs)
          case _ => throw new Exception("Tried to allocate without allocation token")
        }
      }
      case LetDelayInE(param, exp, body) => {
        val expV = eval(exp)
        expV match {
          case ThunkV(thunkExp, env, thunkQs) => {
            val thunkV = eval(thunkExp)(env, thunkQs) // TODO: memoize this
            eval(body)(store + (param -> thunkV), qs + (param -> LaterQ()))
          }
          case _ => throw new Exception("Tried to dereference a non-thunk: " + exp)
        }
      }
      case ConsE(lhs, rhs) => {
        val rhsv = eval(rhs)
        if (!rhsv.isInstanceOf[ThunkV]) {
          throw new Exception("Tried to cons a non-thunk into a stream")
        }
        StreamV(eval(lhs), rhsv.asInstanceOf[ThunkV])
      }
      case LetConsInE(lhs, rhs, exp, body) => {
        val expV = eval(exp)
        expV match {
          case StreamV(v, vs) => eval(body)(store + (lhs -> v, rhs -> vs), addToQs(addToQs(qs, lhs, v), rhs, vs))
          case _ => throw new Exception("Tried to stream-decompose a non-stream")
        }
      }
      case AddE(lhs, rhs) => {
        val lhsv = eval(lhs)
        val rhsv = eval(rhs)
        lhsv match {
          case IntV(lhsi) => rhsv match {
            case IntV(rhsi) => IntV(lhsi + rhsi)
            case _ => throw new Exception("Tried to add non-int rhs")
          }
          case _ => throw new Exception("Tried to add non-int lhs")
        }
      }
      case FixE(param, body) => eval(replace(body)(param, FixE(param, body)))
      case PairE(fst, snd) => PairV(eval(fst), eval(snd))
      case FstE(exp) => {
        val expV = eval(exp)
        expV match {
          case PairV(fst, _) => fst
          case _ => throw new Exception("Tried to extract fst from non-product value")
        }
      }
      case SndE(exp) => {
        val expV = eval(exp)
        expV match {
          case PairV(_, snd) => snd
          case _ => throw new Exception("Tried to extract snd from non-product value")
        }
      }
      case CaseE(exp, inlParam, inlBody, inrParam, inrBody) => {
        val expV = eval(exp)
        expV match {
          case InlV(v) => eval(inlBody)(store + (inlParam -> v), addToQs(qs, inlParam, v))
          case InrV(v) => eval(inrBody)(store + (inrParam -> v), addToQs(qs, inrParam, v))
          case _ => throw new Exception("Tried to match on a non-sum value: " + exp.toString)
        }
      }
      case InlE(exp) => InlV(eval(exp))
      case InrE(exp) => InrV(eval(exp))
      case IntoE(exp) => IntoV(eval(exp))
      case OutE(exp) => {
        val expV = eval(exp)
        expV match {
          case IntoV(v) => v
          case _ => throw new Exception("Tried to out a non-into value: " + exp.toString)
        }
      }
      case TokenE() => TokenV()
    }
  }

  def test(exp: Expression, v: Value, store: Store = Map.empty): Unit = {
    val result = eval(exp)(store, Map.empty)
    assert(result == v, "\nExpected: " + v + "\nActual: " + result)
  }
  val tokenEnv: Store = Map.empty + ("t" -> TokenV())
  def testToken(exp: Expression, v: Value, store: Store = tokenEnv): Unit = test(exp, v, store)

  test(IntE(1), IntV(1))
  test(LambdaE("x", VarE("x")), LambdaV("x", VarE("x"), Map.empty, Map.empty))
  test(AppE(LambdaE("x", VarE("x")), IntE(42)), IntV(42))
  test(PromoteE(IntE(42)), StableV(IntV(42)))
  test(LetStableInE("x", PromoteE(IntE(42)), VarE("x")), IntV(42))
  testToken(DelayE(VarE("t"), IntE(42)), ThunkV(IntE(42), tokenEnv, Map.empty))
  testToken(LetDelayInE("x", DelayE(VarE("t"), IntE(42)), VarE("x")), IntV(42))
  testToken(ConsE(IntE(42), DelayE(VarE("t"), IntE(21))), StreamV(IntV(42), ThunkV(IntE(21), tokenEnv, Map.empty)))
  testToken(LetConsInE("v", "vs", ConsE(IntE(42), DelayE(VarE("t"), IntE(21))), VarE("v")), IntV(42))

  println("All tests passed")
  private val allocStream = FixE("@s", ConsE(TokenE(), DelayE(TokenE(), VarE("@s"))))


  def tickStore(store: Store, qs: Qualifiers): Store = {
    store.map(entry => {
      val (key, value) = entry
      qs.getOrElse(key, throw new Exception("Tried to get non-existent qualifier: " + key)) match {
        case LaterQ() => {
          value match {
            case ThunkV(body, env, qs) => {
              Some((key, eval(body)(env, qs)))
            }
            case _ => Some(entry)
          }
        }
        case StableQ() => Some(entry)
        case NowQ() => None
      }
    }).flatten.toMap
  }

  private def tickQualifiers(qs: Qualifiers): Qualifiers = {
    qs.map(entry => {
      entry._2 match {
        case LaterQ() => Some((entry._1, NowQ()))
        case StableQ() => Some(entry)
        case NowQ() => None
      }
    }).flatten.toMap
  }

  def run(program: Expression, input: List[Expression]): Unit = {
    println(TypeChecker.checkType(program))
    val programWithInput = input.foldLeft(AppE(program, allocStream))((acc, inputItem) => {
      AppE(acc, inputItem)
    })

    var res = eval(programWithInput)(Map.empty, Map.empty).asInstanceOf[StreamV]
    while (true) {
      println(res.v)
      Thread.sleep(1000)
      val newStore = tickStore(res.vs.env, res.vs.qs)
      val newQs = tickQualifiers(res.vs.qs)
      println("store size: " + newStore.size)
      res = eval(res.vs.body)(newStore, newQs).asInstanceOf[StreamV]
    }
  }
}
