package main.scala

import scala.collection.mutable.Set

package object adjs {
  type Store = Map[String, Value]
  type TypeStore = Map[String, Type]
  type TypeRuleStore = Set[(Type, Type)]
  type Qualifiers = Map[String, Qualifier]
}