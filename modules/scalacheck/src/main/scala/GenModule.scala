package scalaz

package schema

package scalacheck

import org.scalacheck._, Arbitrary._

import GenModule._

trait GenModule extends SchemaModule {

  def representation(primNt: Prim ~> Gen) = new Schema.Representation[Gen] {
    val prims = primNt

    def handleList[A] = (gen => Gen.listOf[A](gen))

    val handleRecord = NaturalTransformation.refl[Gen]

    val handleUnion = NaturalTransformation.refl[Gen]

    def labelBranch(label: SumTermId) = NaturalTransformation.refl[Gen]

    def labelField(label: ProductTermId) = NaturalTransformation.refl[Gen]

    def zero: Gen[Unit] = Gen.const(())
  }

  trait ToGen[S[_]] {
    def toGen: S ~> Gen
  }

  implicit class ToGenOps[A](schema: Schema[A]) {

    def toGen(implicit primToGen: Prim ~> Gen): Gen[A] =
      Schema.covariantFold(representation(primToGen)).apply(schema)
  }

}

object GenModule {

  implicit val genAp: Applicative[Gen] = new Applicative[Gen] {
    override def ap[T, U](fa: => Gen[T])(f: => Gen[T => U]): Gen[U] =
      fa.flatMap(a => f.map(_(a)))
    override def point[T](a: => T): Gen[T] = Gen.const(a)
  }

  implicit val genAlt: Alt[Gen] = new Alt[Gen] {
    override def point[A](a: => A): org.scalacheck.Gen[A] = Gen.const(a)
    override def ap[A, B](fa: => org.scalacheck.Gen[A])(
      f: => org.scalacheck.Gen[A => B]
    ): org.scalacheck.Gen[B] = genAp.ap(fa)(f)
    override def alt[X](fa: => Gen[X], fb: => Gen[X]): Gen[X] =
      for {
        a <- fa
        b <- fb
        x <- Gen.oneOf(a, b)
      } yield x
  }

}
