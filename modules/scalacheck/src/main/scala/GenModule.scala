package scalaz

package schema

package scalacheck

import org.scalacheck.Gen

trait GenModule extends SchemaModule {

  def toGen: Schema ~> Gen = new (Schema ~> Gen) {
    override def apply[A](schema: Schema[A]): Gen[A] = schema match {
      case _: Schema.PrimSchema[_]        => ???
      case record: Schema.RecordSchema[_] => recordGen(record)
      case union: Schema.Union[_]         => unionGen(union)
      case seq: Schema.SeqSchema[_]       => seqGen(seq)
    }
  }

  def recordGen[A](schema: Schema.RecordSchema[A]): Gen[A] = {
    schema.terms.map {
      case essential: Schema.Field.Essential[_, _]       => toGen(essential.base)
      case nonEssential: Schema.Field.NonEssential[_, _] => Gen.option(toGen(nonEssential.base))
    }

    ???
  }

  def unionGen[A](schema: Schema.Union[A]): Gen[A] = {
    val branchGens = schema.terms.map(term => branchGen(term))
    branchGens.tail.headOption
      .fold(branchGens.head)(g => Gen.oneOf(branchGens.head, g, branchGens.tail.toList.tail: _*))
  }

  def branchGen[A, A0](branch: Schema.Branch[A, A0]): Gen[A] =
    toGen[A0](branch.base).map(branch.prism.reverseGet)

  def seqGen[A](schema: Schema.SeqSchema[A]): Gen[List[A]] =
    Gen.listOf(toGen(schema.element))
}
