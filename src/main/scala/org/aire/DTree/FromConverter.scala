package org.aire.DTree

trait FromConverter extends FromImplicitConversions with LowPriorityConversion {
  self: FromBuilder with NodeTypes =>
    implicit def ncLeaf[A <: LeafNode]: LeafConversion[A] = new LeafConversion[A] {}
}

trait FromImplicitConversions {
  self: NodeTypes with FromBuilder =>
  trait NodeConverter[Child <: Node] {
    protected final type SelfChild = Child
    protected type ReturnChild <: SelfChild
    protected type KeySet[X] = X
    type Return[X]
    def from[X](keys : KeySet[X]) : Return[X]
  }
  trait BranchConversion[S <: Node] extends NodeConverter[S] {
    override protected type ReturnChild = S
    override type Return[X] = FromBranch[S, KeySet[X]]
    def from[X](keys : KeySet[X]) : Return[X] = new FromBranch[S, KeySet[X]](keys)
  }
  trait LeafConversion[S <: LeafNode] extends NodeConverter[S] {
    override protected type ReturnChild = S
    override type Return[X] = FromLeaf[S, KeySet[X]]

    def from[X](keys : KeySet[X]) : Return[X] = new FromLeaf[S, KeySet[X]](keys)
  }
}

trait LowPriorityConversion {
  self: FromImplicitConversions with NodeTypes =>
  implicit def ncChild[A <: Node]: BranchConversion[A] = new BranchConversion[A] {}
}