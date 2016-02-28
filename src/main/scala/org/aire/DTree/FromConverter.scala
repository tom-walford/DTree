package org.aire.DTree

import shapeless.HList

trait FromConverter extends FromImplicitConversions with LowPriorityConversion {
  self: FromBuilder with NodeTypes =>
    implicit def ncLeaf[A <: LeafNode]: LeafConversion[A] = new LeafConversion[A] {}
}

trait FromImplicitConversions {
  self: NodeTypes with FromBuilder =>
  trait NodeConverter[Child] {
    protected final type SelfChild = Child
    protected type ReturnChild <: SelfChild
    type Return[T <: Node, X <: SelfChild, H <: HList]
    def from[N <: Node, H <: HList](keys : H) : Return[N, ReturnChild, H]
  }
  trait BranchConversion[S <: Node] extends NodeConverter[S] {
    override protected type ReturnChild = S
    override type Return[T <: Node, X <: SelfChild, H <: HList] = FromBranch[T, X, H]
    def from[N <: Node, H <: HList](keys : H): Return[N, S, H] = new FromBranch[N, S, H](keys)
  }
  trait LeafConversion[S <: LeafNode] extends NodeConverter[S] {
    override protected type ReturnChild = S
    override type Return[T <: Node, X <: SelfChild, H <: HList] = FromLeaf[T, X, H]

    def from[N <: Node, H <: HList](keys: H): Return[N, S, H] = new FromLeaf[N, S, H](keys)
  }
}

trait LowPriorityConversion {
  self: FromImplicitConversions with NodeTypes =>
  implicit def ncChild[A <: Node]: BranchConversion[A] = new BranchConversion[A] {}
}