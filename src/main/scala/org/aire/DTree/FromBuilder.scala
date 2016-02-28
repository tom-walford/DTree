package org.aire.DTree

import shapeless.{HNil, ::, HList}

trait FromBuilder {
  self: NodeTypes with FromConverter =>
  sealed trait From extends Query {
    override type Value = RootValue
    override def morph: Option[RootValue] => Option[Value] = i => i
  }

  case class FromBranch[Root <: Node, S <: Node, K <: HList](val keys : K) extends From {
    override type Tree = Root
    override type RootValue = S#Value
    def andThen(key : S#Child#Value#Key)(implicit nc: NodeConverter[S#Child]) = nc.from[Tree, S#Child#Value#Key :: K](key :: keys)
  }

  case class FromLeaf[Root <: Node, S <: LeafNode, K <: HList](val keys : K) extends From {
    override type Tree = Root
    override type RootValue = S#Value
  }

  trait FromNode[T <: Node] {
    def from(key : T#Value#Key) : FromBranch[T,T, T#Value#Key :: HNil] = new FromBranch[T,T,T#Value#Key :: HNil](key :: HNil)
  }
}