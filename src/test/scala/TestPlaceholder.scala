import org.aire.DTree.DTree._
import org.aire.DTree.KeyedEntity
import org.aire.DTree.utils.FlattenTuple

import scala.concurrent.Future

object TestPlaceholder {

  trait A extends KeyedEntity[Int]
  trait B extends KeyedEntity[Int]
  trait C extends KeyedEntity[String]

  val k = new DTree[C ->: B ->: A ->: TNil] {
    def build[A, B, P](keys: P, mapFunc: Option[A] => Option[B]): Future[B] = ???
  }

  k.from("3").andThen(2)
    .map(_ => "")
    .flatMap(_ => Some(1)).eval(k)

  val p = k.from("3").andThen(1).andThen(3).keys
  FlattenTuple(((1,2),3))
  val o = FlattenTuple(p)

  FlattenTuple(k.from("3").andThen(2).andThen(3).keys)._1 == "3"
}
