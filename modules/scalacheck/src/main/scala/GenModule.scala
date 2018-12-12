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
        case prim: Schema.PrimSchema[_]        => primToGen(prim.prim)
        case record: Schema.RecordSchema[_, _] => recordGen(record)
        case union: Schema.Union[_, _]         => unionGen(union)
        case seq: Schema.SeqSchema[_]          => seqGen(seq)
        case iso: Schema.IsoSchema[_, _]       => schemaToGen(primToGen)(iso.base).map(iso.iso.get)
        case opt: Schema.OptionalSchema[_]     => Gen.option(schemaToGen(primToGen)(opt.base))
      }
    }

  def nt[ID, A](implicit primToGen: Prim ~> Gen): (Schema.Term[ID, A, ?] ~> Gen[?]) =
    new (Schema.Term[ID, A, ?] ~> Gen[?]) {
      override def apply[X](branch: Schema.Term[ID, A, X]): Gen[X] =
        schemaToGen(primToGen)(branch.base)
    }

  private def recordGen[A, AP](
    schema: Schema.RecordSchema[A, AP]
  )(
    implicit
    primToGen: Prim ~> Gen
  ): Gen[A] = FreeAp2.covariantFold(schema.fields)(nt[ProductTermId, A]).map(schema.g)

  private def unionGen[A, AE](
    schema: Schema.Union[A, AE]
  )(implicit primToGen: Prim ~> Gen): Gen[A] =
    FreeChoice.covariantFold(schema.choices)(nt[SumTermId, A]).map(schema.g)

  private def seqGen[A](
    schema: Schema.SeqSchema[A]
  )(
    implicit
    primToGen: Prim ~> Gen
  ): Gen[List[A]] =
    Gen.listOf(schemaToGen(primToGen)(schema.element))
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
