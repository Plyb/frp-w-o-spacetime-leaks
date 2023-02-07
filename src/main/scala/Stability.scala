package main.scala

object Stability {
  def isStable(v: Value): Boolean = {
    v match {
      case IntV(_) => true
      case StableV(_) => true
      case LambdaV(_, _, _, qs) => {
        qs.forall(entry => entry._2 == StableQ())
      }
      case PairV(fst, snd) => {
        isStable(fst) && isStable(snd)
      }
      case InlV(v) => isStable(v)
      case InrV(v) => isStable(v)
      case _ => false
    }
  }
}
