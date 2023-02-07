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
case class ProductT(fst: Type, snd: Type) extends Type
case class SumT(inl: Type, inr: Type) extends Type
case class TempRecT(body: Type) extends Type
case class TempRecVarT(id: Int) extends Type
case class AllocT() extends Type
case class TypeVariable(id: Int) extends Type
case class ErrorT(msg: String) extends Type

object CompositeTypes {
  def EventT(t: Type) = TempRecT(SumT(t, TempRecVarT(0)))
}

object TypeChecker {
  var typeVariable = -1
  var typeRules: TypeRuleStore = Set.empty
  val stableJudgements: Set[Type] = Set.empty

  def checkType(exp: Expression) = {
    val overallType = buildRules(exp, Map.empty) // Add initial env
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
      case FixE(param, body) => {
        val paramT = newTypeVariable(param)
        buildRules(body, env + (param -> paramT))
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
      case PairE(fst, snd) => {
        val fstT = buildRules(fst, env)
        val sndT = buildRules(snd, env)
        ProductT(fstT, sndT)
      }
      case FstE(exp) => {
        val expT = buildRules(exp, env)
        val fstT = newTypeVariable("@fst")
        val sndT = newTypeVariable("@snd")
        typeRules.add((expT, ProductT(fstT, sndT)))
        fstT
      }
      case SndE(exp) => {
        val expT = buildRules(exp, env)
        val fstT = newTypeVariable("@fst")
        val sndT = newTypeVariable("@snd")
        typeRules.add((expT, ProductT(fstT, sndT)))
        sndT
      }
      case CaseE(exp, inlParam, inlBody, inrParam, inrBody) => {
        val expT = buildRules(exp, env)
        val inlParamT = newTypeVariable(inlParam)
        val inlBodyT = buildRules(inlBody, env + (inlParam -> inlParamT))
        val inrParamT = newTypeVariable(inrParam)
        val inrBodyT = buildRules(inrBody, env + (inrParam -> inrParamT))
        typeRules.add((expT, SumT(inlParamT, inrParamT)))
        typeRules.add((inlBodyT, inrBodyT))
        inlBodyT
      }
      case InlE(exp) => {
        val expT = buildRules(exp, env)
        val inrT = newTypeVariable("@inr")
        SumT(expT, inrT)
      }
      case InrE(exp) => {
        val expT = buildRules(exp, env)
        val inlT = newTypeVariable("@inl")
        SumT(inlT, expT)
      }
      case IntoE(exp) => {
        val expT = buildRules(exp, env)
        decurType(expT)
      }
      case OutE(exp) => {
        val expT = buildRules(exp, env)
        typeRules.add((expT, TempRecT(newTypeVariable("@temp-rec"))))
        recurType(expT)(expT, 0)
      }
    }
  }

  private def recurType(t: Type)(implicit replacement: Type, deBruijnLevel: Int): Type = {
    t match {
      case IntT() => IntT()
      case LambdaT(param, ret) => LambdaT(recurType(param), recurType(ret))
      case CircleT(t) => CircleT(recurType(t))
      case SquareT(t) => SquareT(recurType(t))
      case StreamT(t) => StreamT(recurType(t))
      case ProductT(fst, snd) => ProductT(recurType(fst), recurType(snd))
      case SumT(inl, inr) => SumT(recurType(inl), recurType(inr))
      case TempRecT(body) => recurType(body)(replacement, deBruijnLevel + 1)
      case TempRecVarT(id) => if (id == deBruijnLevel) CircleT(replacement) else t
      case _ => t
    }
  }

  private def decurType(t: Type): Type = {
    val body = t match {
      case CircleT(TempRecT(body)) => TempRecVarT(0)
      case IntT() => IntT()
      case LambdaT(param, ret) => LambdaT(decurType(param), decurType(ret))
      case CircleT(t) => CircleT(decurType(t))
      case SquareT(t) => SquareT(decurType(t))
      case StreamT(t) => StreamT(decurType(t))
      case ProductT(fst, snd) => ProductT(decurType(fst), decurType(snd))
      case SumT(inl, inr) => SumT(decurType(inl), decurType(inr))
      case TempRecT(body) => decurType(body)
      case _ => t
    }
    TempRecT(body)
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
      case LambdaT(param, ret) => stable(param) && stable(ret)
      case ProductT(fst, snd) => stable(fst) && stable(snd)
      case SumT(inl, inr) => stable(inl) && stable(inr)
      case TempRecT(body) => stable(body)
      case TempRecVarT(_) => true
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
        case ProductT(fst, snd) if t2.isInstanceOf[ProductT] => {
          val t2Product = t2.asInstanceOf[ProductT]
          typeEqual(fst, t2Product.fst) && typeEqual(snd, t2Product.snd)
        }
        case SumT(inl, inr) if t2.isInstanceOf[SumT] => {
          val t2Sum = t2.asInstanceOf[SumT]
          typeEqual(inl, t2Sum.inl) && typeEqual(inr, t2Sum.inr)
        }
        case TempRecT(body) if t2.isInstanceOf[TempRecT] => typeEqual(body, t2.asInstanceOf[TempRecT].body)
        case TempRecVarT(id) if t2.isInstanceOf[TempRecVarT] => id == t2.asInstanceOf[TempRecVarT].id
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
      case ProductT(fst, snd) if rhs.isInstanceOf[ProductT] => {
        val rhsProduct = rhs.asInstanceOf[ProductT]
        List((fst, rhsProduct.fst), (snd, rhsProduct.snd))
      }
      case SumT(inl, inr) if rhs.isInstanceOf[SumT] => {
        val rhsSum = rhs.asInstanceOf[SumT]
        List((inl, rhsSum.inl), (inr, rhsSum.inr))
      }
      case TempRecT(body) if rhs.isInstanceOf[TempRecT] => {
        val rhsTempRec = rhs.asInstanceOf[TempRecT]
        List((body, rhsTempRec.body))
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
      case ProductT(fst, snd) => ProductT(getSubstitutionsIn(fst), getSubstitutionsIn(snd))
      case SumT(inl, inr) => SumT(getSubstitutionsIn(inl), getSubstitutionsIn(inr))
      case CircleT(t) => CircleT(getSubstitutionsIn(t))
      case SquareT(t) => SquareT(getSubstitutionsIn(t))
      case StreamT(t) => StreamT(getSubstitutionsIn(t))
      case TempRecT(body) => TempRecT(getSubstitutionsIn(body))
      case TempRecVarT(id) => t
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
      case ProductT(fst, snd) => isPolyType(fst) || isPolyType(snd)
      case SumT(inl, inr) => isPolyType(inl) || isPolyType(inr)
      case CircleT(t) => isPolyType(t)
      case SquareT(t) => isPolyType(t)
      case StreamT(t) => isPolyType(t)
      case TempRecT(body) => isPolyType(body)
      case TempRecVarT(id) => false
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
