package org.aire.DTree

import shapeless.{HNil, ::, HList}

trait FromBuilder {
  self: NodeTypes with FromConverter =>
  sealed trait From extends Query {
    override type Value = CurrentNode#Value
    override def morph: Option[CurrentNode#Value] => Option[Value] = i => i
  }

  case class FromBranch[S <: Node, K <: S#Value#Key :: HList](val keys : K) extends From {
    override type CurrentNode = S
    def andThen(key : CurrentNode#Child#Value#Key)(implicit nc: NodeConverter[CurrentNode#Child]) = nc.from(key :: keys)
  }

  case class FromLeaf[S <: LeafNode, K <: S#Value#Key :: HList](val keys : K) extends From {
    override type CurrentNode = S
  }

  trait FromNode[T <: Node] {
    def from(key : T#Value#Key) : FromBranch[T, T#Value#Key :: HNil] = new FromBranch[T,T#Value#Key :: HNil](key :: HNil)
  }
}