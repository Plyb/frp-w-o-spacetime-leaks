package main.scala
import adjs.Store
import adjs.Qualifiers

object main extends App {

  // Examples
  val const = FixE("const", LambdaE("ts", LambdaE("n",
    LetConsInE("t", "delayed_ts_prime", VarE("ts"),
      LetDelayInE("ts_prime", VarE("delayed_ts_prime"),
        LetStableInE("x", PromoteE(VarE("n")),
          ConsE(VarE("x"), DelayE(VarE("t"), AppE(AppE(VarE("const"), VarE("ts_prime")), VarE("x"))))
        )))
  )))

  val sum_acc = FixE("sum_acc", LambdaE("ts", LambdaE("ns", LambdaE("acc",
    LetConsInE("t", "delayed_ts_prime", VarE("ts"),
      LetDelayInE("ts_prime", VarE("delayed_ts_prime"),
        LetConsInE("n", "delayed_ns_prime", VarE("ns"),
          LetDelayInE("ns_prime", VarE("delayed_ns_prime"),
            LetStableInE("x", PromoteE(AddE(VarE("n"), VarE("acc"))),
              ConsE(VarE("x"), DelayE(VarE("t"), AppE(AppE(AppE(VarE("sum_acc"), VarE("ts_prime")), VarE("ns_prime")), VarE("x"))))
            )))))
  ))))
  val sum = LambdaE("ts", LambdaE("ns",
    AppE(AppE(AppE(VarE("sum_acc"), VarE("ts")), VarE("ns")), IntE(0))
  ))
  val sum2 = AppE(LambdaE("sum_acc", sum), sum_acc)

  val tails = FixE("tails", LambdaE("ts", LambdaE("xs",
    LetConsInE("t", "delayed_ts_prime", VarE("ts"),
      LetDelayInE("ts_prime", VarE("delayed_ts_prime"),
        LetConsInE("n", "delayed_xs_prime", VarE("xs"),
          LetDelayInE("xs_prime", VarE("delayed_xs_prime"),
            ConsE(VarE("xs"), DelayE(VarE("t"), AppE(AppE(VarE("tails"), VarE("ts_prime")), VarE("xs_prime"))))
          ))))
  )))

  val map = FixE("map", LambdaE("ts", LambdaE("h", LambdaE("xs",
    LetConsInE("t", "delayed_ts_prime", VarE("ts"),
      LetDelayInE("ts_prime", VarE("delayed_ts_prime"),
        LetConsInE("x", "delayed_xs_prime", VarE("xs"),
          LetDelayInE("xs_prime", VarE("delayed_xs_prime"),
            LetStableInE("f", VarE("h"),
              ConsE(AppE(VarE("f"), VarE("x")),
                DelayE(VarE("t"), AppE(AppE(AppE(VarE("map"), VarE("ts_prime")), VarE("h")), VarE("xs_prime")))))))))
  ))))

  val unfold = FixE("unfold", LambdaE("ts", LambdaE("h", LambdaE("x",
    LetConsInE("t", "delayed_ts_prime", VarE("ts"),
      LetDelayInE("ts_prime", VarE("delayed_ts_prime"),
        LetStableInE("f", VarE("h"),
          AppE(LambdaE("pair",
            LetDelayInE("x_prime", SndE(VarE("pair")),
              ConsE(FstE(VarE("pair")),
                DelayE(VarE("t"), AppE(AppE(AppE(VarE("unfold"),
                  VarE("ts_prime")),
                  VarE("h")),
                  VarE("x_prime")))))
          ), AppE(VarE("f"), VarE("x"))))))
  ))))

  val switch = FixE("switch", LambdaE("ts", LambdaE("xs", LambdaE("e",
    LetConsInE("t", "delayed_ts_prime", VarE("ts"),
      LetDelayInE("ts_prime", VarE("delayed_ts_prime"),
        LetConsInE("x", "delayed_xs_prime", VarE("xs"),
          LetDelayInE("xs_prime", VarE("delayed_xs_prime"),
            CaseE(OutE(VarE("e")),
              "ys", VarE("ys"),
              "delayed_e_prime", LetDelayInE("e_prime", VarE("delayed_e_prime"),
                ConsE(VarE("x"), DelayE(VarE("t"), AppE(AppE(AppE(VarE("switch"),
                  VarE("ts_prime")),
                  VarE("xs_prime")),
                  VarE("e_prime"))))))))))
  ))))

  // DATA INPUTS
  val constant = ConsE(IntE(1), DelayE(TokenE(), VarE("@n")))
  val time = AppE(LambdaE("@t", FixE("@n",
    ConsE(
      VarE("@t"),
      DelayE(TokenE(), AppE(LambdaE("@t",  VarE("@n")), AddE(VarE("@t"), IntE(1))))))),
    IntE(0))
  val alternating = FixE("@n", ConsE(
    InlE(IntE(42)),
    DelayE(TokenE(), ConsE(
      InrE(IntE(42)),
      DelayE(TokenE(), VarE("@n"))))))
  val threeThenLeft = IntoE(InrE(DelayE(TokenE(),
    IntoE(InrE(DelayE(TokenE(),
      IntoE(InrE(DelayE(TokenE(),
        IntoE(InlE(FixE("@n", ConsE(IntE(42), DelayE(TokenE(), VarE("@n")))))))))))))))

//  Interpreter.run(const, List(IntE(42)))
//  Interpreter.run(sum2, List(time))
//  Interpreter.run(tails, List(time))
//  Interpreter.run(map, List(PromoteE(LambdaE("n", AddE(VarE("n"), IntE(10)))), time))
//  Interpreter.run(unfold, List(
//    PromoteE(LambdaE("x",
//      PairE(VarE("x"), DelayE(TokenE(), AddE(VarE("x"), IntE(1)))))),
//    IntE(0)))
//  Interpreter.run(map, List(PromoteE(LambdaE("x",
//    CaseE(VarE("x"),
//      "y", VarE("y"),
//      "z", IntE(0)),
//  )), alternating))
  Interpreter.run(switch, List(time, threeThenLeft))
}
