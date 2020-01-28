package schemaz

import scalaz.{ Alt, Applicative, ~> }
import org.scalacheck._
import generic.GenericSchemaModule

trait GenericGenModule[R <: Realisation] extends GenericSchemaModule[R] {

  implicit val genApplicativeInstance: Applicative[Gen] = new Applicative[Gen] {
    override def ap[T, U](fa: => Gen[T])(f: => Gen[T => U]): Gen[U] =
      fa.flatMap(a => f.map(_(a)))
    override def point[T](a: => T): Gen[T] = Gen.const(a)
  }

  implicit val genAltInstance: Alt[Gen] = new Alt[Gen] {
    override def point[A](a: => A): org.scalacheck.Gen[A] = Gen.const(a)
    override def ap[A, B](fa: => org.scalacheck.Gen[A])(
      f: => org.scalacheck.Gen[A => B]
    ): org.scalacheck.Gen[B]                                  = genApplicativeInstance.ap(fa)(f)
    override def alt[X](fa: => Gen[X], fb: => Gen[X]): Gen[X] = Gen.oneOf(fa, fb)

  }

  implicit object GenTransform extends Transform[Gen] {
    def apply[A, B](fa: Gen[A], p: NIso[A, B]): Gen[B] = fa.map(p.f)
  }

  implicit final def genericGenInterpreter(
    implicit primNT: realisation.Prim ~> Gen
  ): RInterpreter[Gen] = recursion.Interpreter.cata(
    covariantTargetFunctor(
      primNT,
      λ[Gen ~> λ[X => Gen[List[X]]]](x => Gen.listOf(x)),
      discardingFieldLabel,
      discardingBranchLabel,
      λ[λ[X => () => Gen[X]] ~> Gen](
        thunk =>
          Gen.delay {
            thunk()
          }
      )
    )
  )
}
