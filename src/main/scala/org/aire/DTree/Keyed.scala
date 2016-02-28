package org.aire.DTree

sealed trait Keyed {
  type Key
  def key : Key
}
trait KeyedEntity[K] extends Keyed {
  override type Key = K
}