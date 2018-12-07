package scalaz

package schema

package scalacheck

import org.scalacheck.Gen
import GenModule._

trait GenModule extends SchemaModule {

  trait ToGen[S[_]] {
    def toGen: S ~> Gen
  }

  implicit class ToGenOps[A](schema: Schema[A]) {

    def toGen(implicit primToGen: Prim ~> Gen): Gen[A] =
      schemaToGen(primToGen)(schema)
  }

  private def schemaToGen(implicit primToGen: Prim ~> Gen): Schema ~> Gen =
    new (Schema ~> Gen) {
      override def apply[A](schema: Schema[A]): Gen[A] = schema match {
        case prim: Schema.PrimSchema[_]     => primToGen(prim.prim)
        case record: Schema.RecordSchema[_] => recordGen(record)
        case union: Schema.Union[_, _]      => unionGen(union)
        case seq: Schema.SeqSchema[_]       => seqGen(seq)
        case iso: Schema.IsoSchema[_, _]    => schemaToGen(primToGen)(iso.base).map(iso.iso.get)
      }
    }

  private def recordGen[A](
    schema: Schema.RecordSchema[A]
  )(
    implicit
    primToGen: Prim ~> Gen
  ): Gen[A] = {
    implicit val genAp: Applicative[Gen] = new Applicative[Gen] {
      override def ap[T, U](fa: => Gen[T])(f: => Gen[T => U]): Gen[U] =
        fa.flatMap(a => f.map(_(a)))
      override def point[T](a: => T): Gen[T] = Gen.const(a)
    }

    schema.fields.foldMap(new (Schema.Field[A, ?] ~> Gen) {
      override def apply[B](fa: Schema.Field[A, B]): Gen[B] = fa match {
        case Schema.Field.Essential(_, base, _, _) =>
          schemaToGen(primToGen)(base)
        case Schema.Field.NonEssential(_, base, _) =>
          Gen.option(schemaToGen(primToGen)(base))
      }
    })
  }

  private def unionGen[A, AE](
    schema: Schema.Union[A, AE]
  )(implicit primToGen: Prim ~> Gen): Gen[A] = {
    val nt: (Schema.Branch[A, ?] ~> Gen[?]) = new (Schema.Branch[A, ?] ~> Gen[?]) {
      override def apply[X](branch: Schema.Branch[A, X]): Gen[X] = branchGen(branch)
    }

    val eGen = FreeChoice.fold(schema.choices)(nt)
    eGen.map(schema.g)
  }

  private def branchGen[A, A0](
    branch: Schema.Branch[A, A0]
  )(
    implicit
    primToGen: Prim ~> Gen
  ): Gen[A0] =
    schemaToGen(primToGen)(branch.base)

  private def seqGen[A](
    schema: Schema.SeqSchema[A]
  )(
    implicit
    primToGen: Prim ~> Gen
  ): Gen[List[A]] =
    Gen.listOf(schemaToGen(primToGen)(schema.element))
}

object GenModule {
  implicit val genThing: Thing[Gen] = new Thing[Gen] {

    def choose[A, B](fa: Gen[A], fb: Gen[B]): Gen[Either[A, B]] =
      for {
        b <- Gen.oneOf(true, false)
        r <- if (b) fa.map(Left(_): Either[A, B]) else fb.map(Right(_): Either[A, B])
      } yield r
  }

}
