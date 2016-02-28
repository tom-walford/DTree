package org.aire.DTree

import shapeless.HList

import scala.concurrent.Future

trait NodeTypes {
  sealed trait Node {
    type Child <: Node
    type Value <: Keyed
  }
  trait TreeNode[K, T <: KeyedEntity[K]] extends Node {
    final override type Value = T
    def value: Value
  }
  trait TNil extends Node {
    override type Child = Nothing
    override type Value = Nothing
  }

  type ->:[A <: Keyed, B <: Node] = Node {
    type Child = B
    type Value = A
  }

  protected type LeafNode = Node {
    type Child = TNil
  }
  trait Query extends Morphable {
    self =>
    def keys: HList
    def map[A](fnc: Value => A): Queryable[A,RootValue] = new Queryable[A,RootValue] {
      override val keys = self.keys
      override val morph = self.morph andThen(_.map(fnc))
    }
    def flatMap[A](fnc: Value => Option[A]): Queryable[A,RootValue] = new Queryable[A,RootValue] {
      override val keys = self.keys
      override val morph = self.morph andThen(_.flatMap(fnc))
    }
    def filter(fnc: Value => Boolean): Queryable[Value,RootValue] = new Queryable[Value,RootValue] {
      override val keys = self.keys
      override val morph = self.morph andThen(_.filter(fnc))
    }
  }
  trait Queryable[A, B] extends Query with Evaluable {
    override type Value = A
    override type RootValue = B
  }

  trait Morphable {
    type RootValue
    type Value
    def keys: HList
    def morph: Option[RootValue] => Option[Value]
  }
  trait Evaluable extends Morphable {
    def eval(tree: Builder): Future[Value] = tree.build(keys, morph)
  }
  trait Builder {
    def build[A, B](keys: HList, mapFunc: Option[A] => Option[B]): Future[B]
  }
}
