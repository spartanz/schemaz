package scalaz

package schema

package scalacheck

import org.scalacheck._

trait GenModule extends SchemaModule {

  implicit def representation(implicit primNt: Prim ~> Gen) = new Schema.Representation[Gen] {
    val prims = primNt

    def handleList[A] = Gen.listOf[A](_)

    def unit: Gen[Unit] = Gen.const(())
  }

  implicit val genAp: Applicative[Gen] = new Applicative[Gen] {
    override def ap[T, U](fa: => Gen[T])(f: => Gen[T => U]): Gen[U] =
      fa.flatMap(a => f.map(_(a)))
    override def point[T](a: => T): Gen[T] = Gen.const(a)
  }

  implicit val genAlt: Alt[Gen] = new Alt[Gen] {
    override def point[A](a: => A): org.scalacheck.Gen[A] = Gen.const(a)
    override def ap[A, B](fa: => org.scalacheck.Gen[A])(
      f: => org.scalacheck.Gen[A => B]
    ): org.scalacheck.Gen[B]                                  = genAp.ap(fa)(f)
    override def alt[X](fa: => Gen[X], fb: => Gen[X]): Gen[X] = Gen.oneOf(fa, fb)

  }

}
