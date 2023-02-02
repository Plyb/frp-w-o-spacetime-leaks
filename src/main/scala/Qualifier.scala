package main.scala

sealed trait Qualifier
case class NowQ() extends Qualifier
case class StableQ() extends Qualifier
case class LaterQ() extends Qualifier