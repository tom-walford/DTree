import org.aire.DTree.DTree._
import org.aire.DTree.KeyedEntity
import shapeless.HList

import scala.concurrent.Future

object TestPlaceholder {

  trait A extends KeyedEntity[Int]
  trait B extends KeyedEntity[Int]
  trait C extends KeyedEntity[String]

  val k = new DTree[C ->: B ->: A ->: TNil] {
    def build[A, B](keys: HList, mapFunc: Option[A] => Option[B]): Future[B] = ???
  }

  k.from("3").andThen(2)
    .map(_ => "")
    .flatMap(_ => Some(1)).eval(k)

  k.from("3").andThen(2).keys
}
