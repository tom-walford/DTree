package org.aire.DTree

import shapeless.HList

import scala.concurrent.Future

trait NodeTypes {
  trait Node {
    type Child <: Node
    type Value <: Keyed
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
    def map[A](fnc: Value => A): Queryable[A,Tree,RootValue] = new Queryable[A,Tree,RootValue] {
      override val keys = self.keys
      override val morph = self.morph andThen(_.map(fnc))
    }
    def flatMap[A](fnc: Value => Option[A]): Queryable[A,Tree,RootValue] = new Queryable[A,Tree,RootValue] {
      override val keys = self.keys
      override val morph = self.morph andThen(_.flatMap(fnc))
    }
    def filter(fnc: Value => Boolean): Queryable[Value,Tree,RootValue] = new Queryable[Value,Tree,RootValue] {
      override val keys = self.keys
      override val morph = self.morph andThen(_.filter(fnc))
    }
  }
  trait Queryable[A, B <: Node, C] extends Query with Evaluable {
    override type Value = A
    override type Tree = B
    override type RootValue = C
  }

  trait Morphable {
    type Tree <: Node
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
