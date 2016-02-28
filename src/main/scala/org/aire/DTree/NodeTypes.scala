package org.aire.DTree

import shapeless._

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
    def map[A](fnc: Value => A): Queryable[A,CurrentNode] = new Queryable[A,CurrentNode] {
      override val keys = self.keys
      override val morph = self.morph andThen(_.map(fnc))
    }
    def flatMap[A](fnc: Value => Option[A]): Queryable[A,CurrentNode] = new Queryable[A,CurrentNode] {
      override val keys = self.keys
      override val morph = self.morph andThen(_.flatMap(fnc))
    }
    def filter(fnc: Value => Boolean): Queryable[Value,CurrentNode] = new Queryable[Value,CurrentNode] {
      override val keys = self.keys
      override val morph = self.morph andThen(_.filter(fnc))
    }
  }
  trait Queryable[A, B <: Node] extends Query with Evaluable {
    override type Value = A
    override type CurrentNode = B
  }

  trait Morphable {
    type Value
    type CurrentNode <: Node
    protected def keys: CurrentNode#Value#Key :: HList
    def morph: Option[CurrentNode#Value] => Option[Value]
  }
  trait Evaluable extends Morphable {
    def eval(tree: Builder): Future[Value] = tree.build(keys, morph)
  }
  trait Builder {
    def build[A, B](keys: HList, mapFunc: Option[A] => Option[B]): Future[B]
  }
}
