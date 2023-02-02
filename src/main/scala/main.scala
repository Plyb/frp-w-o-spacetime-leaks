package main.scala
import adjs.Store
import adjs.Qualifiers

object main extends App {
  val yCombinator = LambdaE("f",
    AppE(
      LambdaE("x", AppE(VarE("f"), AppE(VarE("x"), VarE("x")))),
      LambdaE("x", AppE(VarE("f"), AppE(VarE("x"), VarE("x"))))))

  // Note: this function may cause variable name collisions
  def replace(source: Expression)(implicit toReplace: String, replacement: Expression): Expression = {
    source match {
      case VarE(name) if name == toReplace => replacement
      case PromoteE(exp) => PromoteE(replace(exp))
      case ConsE(lhs, rhs) => ConsE(replace(lhs), replace(rhs))
      case DelayE(token, exp) => DelayE(replace(token), replace(exp))
      case LetConsInE(lhs, rhs, exp, body) => LetConsInE(lhs, rhs, replace(exp), replace(body))
      case LetStableInE(param, exp, body) => LetStableInE(param, replace(exp), replace(body))
      case LetDelayInE(param, exp, body) => LetDelayInE(param, replace(exp), replace(body))
      case LambdaE(param, body) => LambdaE(param, replace(body))
      case FunctionE(name, param, body) => FunctionE(name, param, replace(body))
      case AppE(lhs, rhs) => AppE(replace(lhs), replace(rhs))
      case _ => source
    }
  }

  def eval(exp: Expression)(implicit store: Store, qs: Qualifiers): Value = {
    exp match {
      case IntE(v) => IntV(v)
      case VarE(name) => store.getOrElse(name, NullV())
      case LambdaE(param, body) => LambdaV(param, body, store, qs)
      case FunctionE(name, param, body) => RecursiveV(name, param, body, store, qs)
      case AppE(lhs, rhs) => {
        val lhsv = eval(lhs)
        lhsv match {
          case LambdaV(param, body, env, lambdaQs) => {
            val rhsv = eval(rhs)
            eval(body)(env + (param -> rhsv), lambdaQs + (param -> NowQ()))
          }
          case RecursiveV(name, param, body, env, lambdaQs) => {
            val rhsv = eval(rhs)
            eval(body)(env + (param -> rhsv, name -> lhsv),
              lambdaQs + (param -> NowQ(), name -> (if (Stability.isStable(lhsv)) StableQ() else NowQ()))) // How are functions supposed to be stable??
          }
          case _ => throw new Exception("Non-function lhs of application")
        }
      }
      case PromoteE(exp) => {
        val v = eval(exp)
        if (!v.isInstanceOf[Stable]) {
          throw new Exception("Tried to promote non-stable value")
        }
        StableV(v.asInstanceOf[Stable])
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
          case _ => throw new Exception("Tried to dereference a non-thunk")
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
          case StreamV(v, vs) => eval(body)(store + (lhs -> v, rhs -> vs), qs + (lhs -> NowQ(), rhs -> NowQ()))
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
      case FixE(param, body) => eval(replace(body)( param, FixE(param, body)))
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
  test(AppE(FunctionE("f", "x", VarE("f")), IntE(42)),
    RecursiveV("f", "x", VarE("f"), Map.empty, Map.empty))
  test(PromoteE(IntE(42)), StableV(IntV(42)))
  test(LetStableInE("x", PromoteE(IntE(42)), VarE("x")), IntV(42))
  testToken(DelayE(VarE("t"), IntE(42)), ThunkV(IntE(42), tokenEnv, Map.empty))
  testToken(LetDelayInE("x", DelayE(VarE("t"), IntE(42)), VarE("x")), IntV(42))
  testToken(ConsE(IntE(42), DelayE(VarE("t"), IntE(21))), StreamV(IntV(42), ThunkV(IntE(21), tokenEnv, Map.empty)))
  testToken(LetConsInE("v", "vs", ConsE(IntE(42), DelayE(VarE("t"), IntE(21))), VarE("v")), IntV(42))

  println("All tests passed")

  // Examples
  val const = FunctionE("const", "ts", LambdaE("n",
    LetConsInE("t", "delayed_ts_prime", VarE("ts"),
      LetDelayInE("ts_prime", VarE("delayed_ts_prime"),
        LetStableInE("x", PromoteE(VarE("n")),
          ConsE(VarE("x"), DelayE(VarE("t"), AppE(AppE(VarE("const"), VarE("ts_prime")), VarE("x"))))
        )))
  ))
  val sum_acc = FunctionE("sum_acc", "ts", LambdaE("ns", LambdaE("acc",
    LetConsInE("t", "delayed_ts_prime", VarE("ts"),
      LetDelayInE("ts_prime", VarE("delayed_ts_prime"),
        LetConsInE("n", "delayed_ns_prime", VarE("ns"),
          LetDelayInE("ns_prime", VarE("delayed_ns_prime"),
            LetStableInE("x", PromoteE(AddE(VarE("n"), VarE("acc"))),
              ConsE(VarE("x"), DelayE(VarE("t"), AppE(AppE(AppE(VarE("sum_acc"), VarE("ts_prime")), VarE("ns_prime")), VarE("x"))))
            )))))
  )))
  val sum = FunctionE("sum", "ts", LambdaE("ns",
    AppE(AppE(AppE(VarE("sum_acc"), VarE("ts")), VarE("ns")), IntE(0))
  ))
  val tails = FunctionE("tails", "ts", LambdaE("xs",
    LetConsInE("t", "delayed_ts_prime", VarE("ts"),
      LetDelayInE("ts_prime", VarE("delayed_ts_prime"),
        LetConsInE("n", "delayed_xs_prime", VarE("xs"),
          LetDelayInE("xs_prime", VarE("delayed_xs_prime"),
            ConsE(VarE("xs"), DelayE(VarE("t"), AppE(AppE(VarE("tails"), VarE("ts_prime")), VarE("xs_prime"))))
          ))))
  ))

  val constant = ConsE(IntE(1), DelayE(TokenE(), VarE("@n")))
  val time = ConsE(VarE("@t"), DelayE(TokenE(), AppE(LambdaE("@t",  VarE("@n")), AddE(VarE("@t"), IntE(1)))))

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

  def tickQualifiers(qs: Qualifiers): Qualifiers = {
    qs.map(entry => {
      entry._2 match {
        case LaterQ() => Some((entry._1, NowQ()))
        case StableQ() => Some(entry)
        case NowQ() => None
      }
    }).flatten.toMap
  }

  def run(fun: FunctionE, param: Expression, env: List[FunctionE] = List.empty, store: Store = Map.empty): Unit = {
    println(TypeChecker.checkType(fun, env))
    val allocStream = FixE("@s", ConsE(TokenE(), DelayE(TokenE(), VarE("@s"))))
    val startQs = store.map(entry => (entry._1, StableQ()))
    val (completeStore, qs) = env.foldLeft((store, startQs: Qualifiers))((acc, fun) => {
      val res = eval(fun)(acc._1, acc._2)
      (acc._1 + (fun.name -> res), acc._2 + (fun.name -> StableQ()))
    })
    var res = eval(AppE(AppE(fun, allocStream), param))(completeStore, qs).asInstanceOf[StreamV]
    while (true) {
      println(res.v)
      Thread.sleep(1000)
      val newStore = tickStore(res.vs.env, res.vs.qs)
      val newQs = tickQualifiers(res.vs.qs)
      println("store size: " + newStore.size)
      res = eval(res.vs.body)(newStore, newQs).asInstanceOf[StreamV]
    }
  }
  run(sum, FixE("@n", time), List(sum_acc), Map.empty + ("@t" -> IntV(0)))
//  run(const, IntE(42))
}
