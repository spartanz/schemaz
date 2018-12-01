package scalaz

package schema

package scalacheck

import org.scalacheck.Gen

trait GenModule extends SchemaModule {

  //  TODO : pass realisation as a parameter
  def realisation: Realisation

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
        case union: Schema.Union[_]         => unionGen(union)
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
        case Schema.Field.Essential(_, base, _) =>
          schemaToGen(primToGen)(base)
        case Schema.Field.NonEssential(_, base) =>
          Gen.option(schemaToGen(primToGen)(base))
      }
    })
  }

  private def unionGen[A](schema: Schema.Union[A])(implicit primToGen: Prim ~> Gen): Gen[A] = {
    val branchGens = schema.terms.map(term => branchGen(term))
    branchGens.tail.headOption
      .fold(branchGens.head)(
        g => Gen.oneOf(branchGens.head, g, branchGens.tail.toList.tail: _*)
      )
  }

  private def branchGen[A, A0](
    branch: Schema.Branch[A, A0]
  )(
    implicit
    primToGen: Prim ~> Gen
  ): Gen[A] =
    schemaToGen(primToGen)(branch.base).map(realisation.makeOptic(__ ? branch).reverseGet)

  private def seqGen[A](
    schema: Schema.SeqSchema[A]
  )(
    implicit
    primToGen: Prim ~> Gen
  ): Gen[List[A]] =
    Gen.listOf(schemaToGen(primToGen)(schema.element))
}
