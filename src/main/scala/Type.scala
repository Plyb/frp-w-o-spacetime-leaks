package main.scala
import adjs.TypeStore
import adjs.TypeRuleStore
import scala.collection.mutable.Set

sealed trait Type
case class IntT() extends Type
case class LambdaT(param: Type, ret: Type) extends Type
case class CircleT(T: Type) extends Type
case class SquareT(T: Type) extends Type
case class StreamT(T: Type) extends Type
case class AllocT() extends Type
case class TypeVariable(id: Int) extends Type
case class ErrorT(msg: String) extends Type

object TypeChecker {
  var typeVariable = -1
  var typeRules: TypeRuleStore = Set.empty
  val stableJudgements: Set[Type] = Set.empty

  def checkType(exp: Expression, otherFunctions: List[FunctionE] = List.empty) = {
    val env = otherFunctions.foldLeft(Map.empty: TypeStore)((acc, curr) => {
      acc + (curr.name -> buildRules(curr, acc))
    })
    val overallType = buildRules(exp, env) // Add initial env
    buildImplicitRules()
    if (contradictionsExist()) {
      ErrorT("Contradictory type inference")
    } else if (stabilityJudgementsFail()) {
      ErrorT("Stability judgements fail")
    } else {
      getSubstitutionsIn(overallType)
    }
  }

  private def buildRules(exp: Expression, env: TypeStore): Type = {
    exp match {
      case IntE(_) => IntT()
      case VarE(name) => env.getOrElse(name, ErrorT("Undefined variable"))
      case LambdaE(param, body) => {
        val paramT = newTypeVariable(param)
        val bodyT = buildRules(body, env + (param -> paramT))
        LambdaT(paramT, bodyT)
      }
      case PromoteE(exp) => {
        val expT = buildRules(exp, env)
        stableJudgements.add(expT)
        SquareT(expT)
      }
      case ConsE(lhs, rhs) => {
        val lhsT = buildRules(lhs, env)
        val rhsT = buildRules(rhs, env)
        typeRules.add((rhsT, CircleT(StreamT(lhsT))))
        StreamT(lhsT)
      }
      case DelayE(token, exp) => {
        val tokenT = buildRules(token, env)
        val expT = buildRules(exp, env)
        typeRules.add(tokenT, AllocT())
        CircleT(expT)
      }
      case LetConsInE(lhs, rhs, exp, body) => {
        val expT = buildRules(exp, env)
        val lhsT = newTypeVariable(lhs)
        val rhsT = newTypeVariable(rhs)
        typeRules.add((expT, StreamT(lhsT)))
        typeRules.add((rhsT, CircleT(expT)))
        buildRules(body, env + (lhs -> lhsT, rhs -> rhsT))
      }
      case LetStableInE(param, exp, body) => {
        val expT = buildRules(exp, env)
        val paramT = newTypeVariable(param)
        typeRules.add((expT, SquareT(paramT)))
        buildRules(body, env + (param -> paramT))
      }
      case LetDelayInE(param, exp, body) => {
        val expT = buildRules(exp, env)
        val paramT = newTypeVariable(param)
        typeRules.add((expT, CircleT(paramT)))
        buildRules(body, env + (param -> paramT))
      }
      case LambdaE(param, body) => {
        val paramT = newTypeVariable(param)
        LambdaT(paramT, buildRules(body, env + (param -> paramT)))
      }
      case FunctionE(name, param, body) => {
        val nameT = newTypeVariable(name)
        val paramT = newTypeVariable(param)
        LambdaT(paramT, buildRules(body, env + (name -> nameT, param -> paramT)))
      }
      case AppE(lhs, rhs) => {
        val lhsT = buildRules(lhs, env)
        val rhsT = buildRules(rhs, env)
        val resT = newTypeVariable(exp.toString)
        typeRules.add((lhsT, LambdaT(rhsT, resT)))
        resT
      }
      case AddE(lhs, rhs) => {
        val lhsT = buildRules(lhs, env)
        val rhsT = buildRules(rhs, env)
        typeRules.add((lhsT, IntT()))
        typeRules.add((rhsT, IntT()))
        IntT()
      }
    }
  }

  private def stable(t: Type): Boolean = {
    t match {
      case IntT() => true
      case SquareT(_) => true
      case TypeVariable(id) => {
        getSubstitutionFor(t.asInstanceOf[TypeVariable]) match {
          case Some(value) => stable(value)
          case None => true
        }
      }
      case _ => false
    }
  }

  private def stabilityJudgementsFail(): Boolean = {
    stableJudgements.exists(judgment => {
      !stable(judgment)
    })
  }

  private def contradictionsExist(): Boolean = {
    typeRules.exists(rule => {
      typeRules.exists(rule2 => {
        rule._1 == rule2._2 && !typeEqual(rule._2, rule2._2)
      })
    })
  }

  private def typeEqual(t1: Type, t2: Type): Boolean = {
    t2.isInstanceOf[TypeVariable] ||
      (t1 match {
        case IntT() => t1 == t2
        case LambdaT(param, ret) if t2.isInstanceOf[LambdaT] => {
          val t2Lambda = t2.asInstanceOf[LambdaT]
          typeEqual(param, t2Lambda.param) && typeEqual(ret, t2Lambda.ret)
        }
        case CircleT(t) if t2.isInstanceOf[CircleT] => typeEqual(t, t2.asInstanceOf[CircleT].T)
        case SquareT(t) if t2.isInstanceOf[SquareT] => typeEqual(t, t2.asInstanceOf[SquareT].T)
        case StreamT(t) if t2.isInstanceOf[StreamT] => typeEqual(t, t2.asInstanceOf[StreamT].T)
        case AllocT() => t1 == t2
        case TypeVariable(_) => true
        case ErrorT(_) => false
        case _ => false
    })
  }

  private def buildImplicitRules(): Unit = {
    val newTypeRules: TypeRuleStore = typeRules.flatMap(rule => {
      List(Some(rule), simplifyRule(rule), substitutionRule(rule), swapRule(rule)).flatten
    })
    if (newTypeRules.size != typeRules.size) {
      typeRules = newTypeRules
      buildImplicitRules()
    }
  }

  private def swapRule(rule: (Type, Type)): Some[(Type, Type)] = {
    Some((rule._2, rule._1))
  }

  private def simplifyRule(rule: (Type, Type)): List[(Type, Type)] = {
    val (lhs, rhs) = rule
    lhs match {
      case t: CircleT if rhs.isInstanceOf[CircleT] =>
        List((t.T, rhs.asInstanceOf[CircleT].T))
      case t: SquareT if rhs.isInstanceOf[SquareT] =>
        List((t.T, rhs.asInstanceOf[SquareT].T))
      case t: StreamT if rhs.isInstanceOf[StreamT] =>
        List((t.T, rhs.asInstanceOf[StreamT].T))
      case LambdaT(param, ret) if rhs.isInstanceOf[LambdaT] => {
        val rhsLambda = rhs.asInstanceOf[LambdaT]
        List((param, rhsLambda.param), (ret, rhsLambda.ret))
      }
      case _ =>
        List.empty
    }
  }

  private def substitutionRule(rule: (Type, Type)): Option[(Type, Type)] = {
    val (lhs, rhs) = rule
    // If the rule has a variable lhs, and a polytype rhs:
    //   for each variable in rhs, x, check for any rules with x as the lhs and a monotype rhs
    //     replace x with the monotype rhs
    lhs match {
      case TypeVariable(id) if isPolyType(rhs) => {
        Some((lhs, getSubstitutionsIn(rhs)))
      }
      case _ => {
        rhs match {
          case TypeVariable(id) if isPolyType(lhs) => {
            Some(lhs, getSubstitutionsIn(rhs))
          }
          case _ => {
            None
          }
        }
      }
    }
  }

  private def getSubstitutionsIn(t: Type): Type = {
    t match {
      case IntT() => t
      case LambdaT(param, ret) => LambdaT(getSubstitutionsIn(param), getSubstitutionsIn(ret))
      case CircleT(t) => CircleT(getSubstitutionsIn(t))
      case SquareT(t) => SquareT(getSubstitutionsIn(t))
      case StreamT(t) => StreamT(getSubstitutionsIn(t))
      case AllocT() => t
      case TypeVariable(_) => {
        getSubstitutionFor(t.asInstanceOf[TypeVariable]) match {
          case Some(value) => value
          case None => t
        }
      }
      case ErrorT(msg) => t
    }
  }

  private def getSubstitutionFor(typeVariable: TypeVariable) = {
    val substitutes = typeRules.filter(rule => rule._1 == typeVariable &&
      !isPolyType(rule._2)).map(_._2)
    substitutes.lastOption.orElse(typeRules.filter(rule => rule._1 == typeVariable && (rule._2.isInstanceOf[TypeVariable] &&
      typeVariable.id > rule._2.asInstanceOf[TypeVariable].id)).map(_._2).lastOption)
  }

  private def isPolyType(t: Type): Boolean = {
    t match {
      case IntT() => false
      case LambdaT(param, ret) => isPolyType(param) || isPolyType(ret)
      case CircleT(t) => isPolyType(t)
      case SquareT(t) => isPolyType(t)
      case StreamT(t) => isPolyType(t)
      case AllocT() => false
      case TypeVariable(_) => true
      case ErrorT(_) => false
    }
  }

  private def newTypeVariable(name: String) = {
    typeVariable += 1
//    println(typeVariable, name)
    TypeVariable(typeVariable)
  }
}
