package org.aire.DTree

object DTree extends NodeTypes with FromBuilder with FromConverter {
  trait DTree[T <: Node] extends Builder with FromNode[T]
}