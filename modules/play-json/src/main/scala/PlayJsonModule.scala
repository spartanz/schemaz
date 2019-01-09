package scalaz

package schema

package play.json

import Scalaz._
import Liskov._
import _root_.play.api.libs.json._
import _root_.play.api.libs.functional.syntax._

trait PlayJsonModule[R <: Realisation] extends SchemaModule[R] {

  import Schema._

  implicit final private val readsApplicative = new Applicative[JsResult] {

    def point[A](a: => A): JsResult[A] = JsSuccess(a)

    // Shamelessly copied from the play-json library
    def ap[A, B](fa: => JsResult[A])(f: => JsResult[A => B]): JsResult[B] = (f, fa) match {
      case (JsSuccess(f, _), JsSuccess(a, _)) => JsSuccess(f(a))
      case (JsError(e1), JsError(e2))         => JsError(JsError.merge(e1, e2))
      case (JsError(e), _)                    => JsError(e)
      case (_, JsError(e))                    => JsError(e)

    }
  }

  implicit def reads(
    implicit primNT: R.Prim ~> Reads,
    branchLabel: R.SumTermId <~< String,
    fieldLabel: R.ProductTermId <~< String
  ): HAlgebra[RSchema, Reads] = new (RSchema[Reads, ?] ~> Reads) {

    def apply[A](schema: RSchema[Reads, A]): Reads[A] = schema match {
      case One() => Reads.pure(())
      case :+:(left, right) =>
        Reads(
          json =>
            left.reads(json).fold(_ => right.reads(json).map(\/-.apply), a => JsSuccess(-\/(a)))
        )
      case p: :*:[Reads, a, b, R.Prim, R.SumTermId, R.ProductTermId] =>
        p.left.and(p.right)((x: a, y: b) => (x, y))
      case PrimSchema(p)                                               => primNT(p)
      case SumTerm(id, schema)                                         => (JsPath \ branchLabel(id)).read(schema)
      case u: Union[R.Prim, R.SumTermId, R.ProductTermId, Reads, A, a] => u.choices.map(u.iso.get)
      case ProductTerm(id, schema)                                     => (JsPath \ fieldLabel(id)).read(schema)
      case r: Record[R.Prim, R.SumTermId, R.ProductTermId, Reads, A, a] =>
        r.fields.map(r.iso.get)
      case SeqSchema(elem) => // FIXME: naive implementation
        Reads {
          case JsArray(elems) =>
            elems.toList.traverse(elem.reads _)
          case _ => JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.jsarray"))))
        }
      case i: IsoSchema[R.Prim, R.SumTermId, R.ProductTermId, Reads, a0, A] =>
        i.base.map(i.iso.get)
    }
  }

  implicit val temp = new (JsonSchema.Prim ~> Reads) {

    def apply[A](p: JsonSchema.Prim[A]): Reads[A] = p match {
      case JsonSchema.JsonNull =>
        Reads {
          case JsNull => JsSuccess(())
          case _      => JsError("expected 'null'")
        }
      case JsonSchema.JsonBool   => JsPath.read[Boolean]
      case JsonSchema.JsonString => JsPath.read[String]
      case JsonSchema.JsonNumber => JsPath.read[BigDecimal]
    }

  }

}
