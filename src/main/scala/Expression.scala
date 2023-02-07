package main.scala

sealed trait Expression
// Enough for const
case class IntE(v: Int) extends Expression
case class VarE(name: String) extends Expression
case class PromoteE(exp: Expression) extends Expression
case class ConsE(lhs: Expression, rhs: Expression) extends Expression
case class DelayE(token: Expression, exp: Expression) extends Expression
case class LetConsInE(lhs: String, rhs: String, exp: Expression, body: Expression) extends Expression
case class LetStableInE(param: String, exp: Expression, body: Expression) extends Expression
case class LetDelayInE(param: String, exp: Expression, body: Expression) extends  Expression
case class LambdaE(param: String, body: Expression) extends Expression
case class AppE(lhs: Expression, rhs: Expression) extends Expression
case class FixE(param: String, body: Expression) extends Expression

// Enough for sum
case class AddE(lhs: Expression, rhs: Expression) extends Expression

// Enough for unfold
case class PairE(fst: Expression, snd: Expression) extends Expression
case class FstE(exp: Expression) extends Expression
case class SndE(exp: Expression) extends Expression

// Enough for alternating
case class InlE(exp: Expression) extends Expression
case class InrE(exp: Expression) extends Expression
case class CaseE(exp: Expression,
                 inlParam: String, inlBody: Expression,
                 inrParam: String, inrBody: Expression
                ) extends Expression

// Enough for switch
case class IntoE(exp: Expression) extends Expression
case class OutE(exp: Expression) extends Expression

// These are not allowed in the surface language
case class TokenE() extends Expression
