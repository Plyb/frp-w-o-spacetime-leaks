package main.scala

object Stability {
  def isStable(v: Value): Boolean = {
    v match {
      case IntV(_) => true
      case StableV(_) => true
      case RecursiveV(name, _, _, _, qs) => {
        qs.forall(entry => name == entry._1 || entry._2 == StableQ())
      }
      case LambdaV(_, _, _, qs) => {
        qs.forall(entry => entry._2 == StableQ())
      }
      case _ => false
    }
  }
}
