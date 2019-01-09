package scalaz

package schema

package play.json

import Scalaz._
import Liskov._
import _root_.play.api.libs.json._
import _root_.play.api.libs.functional.syntax._

trait PlayJsonModule[R <: Realisation] extends SchemaModule[R] {

  import Schema._

  // This is needed in order to use `traverse` in `reads`.
  implicit private val readsApplicative = new Applicative[JsResult] {

    def point[A](a: => A): JsResult[A] = JsSuccess(a)

    def ap[A, B](fa: => JsResult[A])(f: => JsResult[A => B]): JsResult[B] = (f, fa) match {
      // Shamelessly copied from the play-json library
      case (JsSuccess(f, _), JsSuccess(a, _)) => JsSuccess(f(a))
      case (JsError(e1), JsError(e2))         => JsError(JsError.merge(e1, e2))
      case (JsError(e), _)                    => JsError(e)
      case (_, JsError(e))                    => JsError(e)

    }
  }

  // This is needed to allow undefined optional fields to be treated as `None`.
  final private def undefinedAsNull[A](field: String, r: Reads[A]): Reads[A] = Reads { json =>
    (json \ field) match {
      case JsDefined(v) => r.reads(v)
      case _            => r.reads(JsNull)
    }
  }

  implicit final def reads(
    implicit primNT: R.Prim ~> Reads,
    branchLabel: R.SumTermId <~< String,
    fieldLabel: R.ProductTermId <~< String
  ): HAlgebra[RSchema, Reads] = new (RSchema[Reads, ?] ~> Reads) {

    def apply[A](schema: RSchema[Reads, A]): Reads[A] = schema match {
      case One() =>
        Reads {
          case JsNull => JsSuccess(())
          case _      => JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.null"))))
        }
      case :+:(left, right) =>
        Reads(
          json =>
            left.reads(json).fold(_ => right.reads(json).map(\/-.apply), a => JsSuccess(-\/(a)))
        )
      case p: :*:[Reads, a, b, R.Prim, R.SumTermId, R.ProductTermId] =>
        p.left.and(p.right)((x: a, y: b) => (x, y))
      case PrimSchema(p)                                               => primNT(p)
      case SumTerm(id, schema)                                         => undefinedAsNull(branchLabel(id), schema)
      case u: Union[R.Prim, R.SumTermId, R.ProductTermId, Reads, A, a] => u.choices.map(u.iso.get)
      case ProductTerm(id, schema)                                     => undefinedAsNull(fieldLabel(id), schema)
      case r: Record[R.Prim, R.SumTermId, R.ProductTermId, Reads, A, a] =>
        r.fields.map(r.iso.get)
      case SeqSchema(elem) =>
        Reads {
          case JsArray(elems) =>
            elems.toList.traverse(elem.reads _)
          case _ => JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.jsarray"))))
        }
      case i: IsoSchema[R.Prim, R.SumTermId, R.ProductTermId, Reads, a0, A] =>
        i.base.map(i.iso.get)
    }
  }

  implicit val jsonPrimReads = new (JsonSchema.Prim ~> Reads) {

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

  implicit final def writes(
    implicit primNT: R.Prim ~> Writes,
    branchLabel: R.SumTermId <~< String,
    fieldLabel: R.ProductTermId <~< String
  ): HAlgebra[RSchema, Writes] =
    new (RSchema[Writes, ?] ~> Writes) {

      def apply[A](schema: RSchema[Writes, A]): Writes[A] = schema match {
        case One()            => Writes(_ => JsNull)
        case :+:(left, right) => Writes(_.fold(left.writes, right.writes))
        case :*:(left, right) =>
          Writes(
            pair =>
              (left.writes(pair._1), right.writes(pair._2)) match {
                case (l @ JsObject(_), r @ JsObject(_)) => l ++ r
                case (l @ JsObject(_), r)               => l ++ Json.obj("_2" -> r)
                case (l, r @ JsObject(_))               => Json.obj("_1" -> l) ++ r
                case (l, r)                             => Json.obj("_1" -> l, "_2" -> r)
              }
          )
        case PrimSchema(p)        => primNT(p)
        case SumTerm(id, s)       => Writes(a => Json.obj(branchLabel(id) -> s.writes(a)))
        case Union(base, iso)     => base.contramap(iso.reverseGet)
        case ProductTerm(id, s)   => Writes(a => Json.obj(fieldLabel(id) -> s.writes(a)))
        case Record(base, iso)    => base.contramap(iso.reverseGet)
        case SeqSchema(elem)      => Writes(seq => JsArray(seq.map(elem.writes(_))))
        case IsoSchema(base, iso) => base.contramap(iso.reverseGet)
      }
    }

  implicit val jsonPrimWrites = new (JsonSchema.Prim ~> Writes) {

    def apply[A](p: JsonSchema.Prim[A]): Writes[A] = p match {
      case JsonSchema.JsonNull   => Writes(_ => JsNull)
      case JsonSchema.JsonBool   => Writes(b => JsBoolean(b))
      case JsonSchema.JsonNumber => Writes(n => JsNumber(n))
      case JsonSchema.JsonString => Writes(s => JsString(s))
    }
  }
}
