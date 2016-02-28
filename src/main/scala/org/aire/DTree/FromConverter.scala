package org.aire.DTree

import shapeless.{::, HList}

trait FromConverter extends FromImplicitConversions with LowPriorityConversion {
  self: FromBuilder with NodeTypes =>
    implicit def ncLeaf[A <: LeafNode]: LeafConversion[A] = new LeafConversion[A] {}
}

trait FromImplicitConversions {
  self: NodeTypes with FromBuilder =>
  trait NodeConverter[Child <: Node] {
    protected final type SelfChild = Child
    protected type ReturnChild <: SelfChild
    protected type KeySet = SelfChild#Value#Key :: HList
    type Return
    def from(keys : KeySet) : Return
  }
  trait BranchConversion[S <: Node] extends NodeConverter[S] {
    override protected type ReturnChild = S
    override type Return = FromBranch[S, KeySet]
    def from(keys : KeySet): Return = new FromBranch[S, KeySet](keys)
  }
  trait LeafConversion[S <: LeafNode] extends NodeConverter[S] {
    override protected type ReturnChild = S
    override type Return = FromLeaf[S, KeySet]

    def from(keys: KeySet): Return = new FromLeaf[S, KeySet](keys)
  }
}

trait LowPriorityConversion {
  self: FromImplicitConversions with NodeTypes =>
  implicit def ncChild[A <: Node]: BranchConversion[A] = new BranchConversion[A] {}
}