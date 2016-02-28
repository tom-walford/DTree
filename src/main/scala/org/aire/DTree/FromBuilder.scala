package org.aire.DTree

trait FromBuilder {
  self: NodeTypes with FromConverter =>
  sealed trait From extends Query {
    override type Value = CurrentNode#Value
    override def morph: Option[CurrentNode#Value] => Option[Value] = i => i
  }

  case class FromBranch[S <: Node, P](val keysPre: P) extends From {
    override type Keys = P
    override val keys = keysPre
    override type CurrentNode = S
    def andThen(key : CurrentNode#Child#Value#Key)(implicit nc: NodeConverter[CurrentNode#Child]) = nc.from(keys -> key)
  }

  case class FromLeaf[S <: LeafNode, P](val keysPre: P) extends From {
    override type Keys = P
    override val keys = keysPre
    override type CurrentNode = S
  }

  trait FromNode[T <: Node] {
    def from(key : T#Value#Key) : FromBranch[T, T#Value#Key] = new FromBranch[T,T#Value#Key](key)
  }
}