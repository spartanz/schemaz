package scalaz

package schema

package play.json

import Scalaz._
import Liskov._
import _root_.play.api.libs.json._
import _root_.play.api.libs.functional.syntax._

trait PlayJsonModule[R <: Realisation] extends SchemaModule[R] {

  import SchemaF._

  type LabelledSchema[A] = (Boolean, FSchema[R.Prim, R.SumTermId, R.ProductTermId, A])

  private def ascribeWith(
    label: Boolean
  ): FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?] ~> LabelledSchema =
    new (FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?] ~> LabelledSchema) {

      def apply[A](fschema: FSchema[R.Prim, R.SumTermId, R.ProductTermId, A]): LabelledSchema[A] =
        (label, fschema)
    }

  final val labelRecordFields: HCoalgebra[HEnvT[Boolean, RSchema, ?[_], ?], LabelledSchema] =
    new (LabelledSchema ~> HEnvT[Boolean, RSchema, LabelledSchema, ?]) {

      def apply[A](seed: LabelledSchema[A]): HEnvT[Boolean, RSchema, LabelledSchema, A] =
        seed match {
          case (x, Fix(r @ Record(_, _))) => HEnvT(x, r.hmap(ascribeWith(true)))
          case (x, Fix(:*:(left, right))) =>
            HEnvT(
              x,
              :*:(
                ascribeWith(false)(left),
                ascribeWith(x)(right)
              )
            )
          case (x, s) => HEnvT(x, s.unFix.hmap(ascribeWith(x)))
        }
    }

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
    ((json \ field) match {
      case JsDefined(v) => r.reads(v)
      case _            => r.reads(JsNull)
    }).repath(JsPath \ field)
  }

  final private val labellingSeed =
    new (FSchema[R.Prim, R.SumTermId, R.ProductTermId, ?] ~> LabelledSchema) {

      def apply[A](fSchema: FSchema[R.Prim, R.SumTermId, R.ProductTermId, A]): LabelledSchema[A] =
        (false, fSchema)
    }

  implicit final def reads(
    implicit primNT: R.Prim ~> Reads,
    branchLabel: R.SumTermId <~< String,
    fieldLabel: R.ProductTermId <~< String
  ): RInterpreter[Reads] =
    new Interpreter[R.Prim, R.SumTermId, R.ProductTermId, Reads] {

      val alg = new (HEnvT[Boolean, RSchema, Reads, ?] ~> Reads) {

        def apply[A](schema: HEnvT[Boolean, RSchema, Reads, A]): Reads[A] = schema.fa match {
          case One() =>
            Reads {
              case JsNull => JsSuccess(())
              case _      => JsError(Seq(JsPath -> Seq(JsonValidationError("error.expected.null"))))
            }
          case :+:(left, right) =>
            Reads(
              json =>
                left
                  .reads(json)
                  .fold(
                    el =>
                      right.reads(json).map(\/-.apply) match {
                        case JsError(er) => JsError(JsError.merge(el, er))
                        case x           => x
                      },
                    a => JsSuccess(-\/(a))
                  )
            )
          case p: :*:[Reads, a, b, R.Prim, R.SumTermId, R.ProductTermId] =>
            if (schema.ask)
              p.left.and(p.right)((x: a, y: b) => (x, y))
            else
              (JsPath \ "_1")
                .read(p.left)
                .and((JsPath \ "_2").read(p.right))((x: a, y: b) => (x, y))
          case PrimSchema(p)       => primNT(p)
          case SumTerm(id, schema) => undefinedAsNull(branchLabel(id), schema)
          case u: Union[R.Prim, R.SumTermId, R.ProductTermId, Reads, A, a] =>
            u.choices.map(u.iso.get)
          case ProductTerm(id, schema) => undefinedAsNull(fieldLabel(id), schema)
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

      def interpret = hyloNT(labelRecordFields, alg).compose(labellingSeed)
    }

  implicit final def writes(
    implicit primNT: R.Prim ~> Writes,
    branchLabel: R.SumTermId <~< String,
    fieldLabel: R.ProductTermId <~< String
  ): RInterpreter[Writes] =
    new Interpreter[R.Prim, R.SumTermId, R.ProductTermId, Writes] {

      val alg = new (HEnvT[Boolean, RSchema, Writes, ?] ~> Writes) {

        def apply[A](env: HEnvT[Boolean, RSchema, Writes, A]): Writes[A] = env.fa match {
          case One()            => Writes(_ => JsNull)
          case :+:(left, right) => Writes(_.fold(left.writes, right.writes))
          case :*:(left, right) =>
            if (env.ask)
              Writes(
                pair =>
                  (left.writes(pair._1), right.writes(pair._2)) match {
                    case (l @ JsObject(_), r @ JsObject(_)) => l ++ r
                    // the following case is impossible, but scalac cannot know that.
                    case (l, r) => Json.obj("_1" -> l, "_2" -> r)
                  }
              )
            else
              Writes(pair => Json.obj("_1" -> left.writes(pair._1), "_2" -> right.writes(pair._2)))
          case PrimSchema(p)        => primNT(p)
          case SumTerm(id, s)       => Writes(a => Json.obj(branchLabel(id) -> s.writes(a)))
          case Union(base, iso)     => base.contramap(iso.reverseGet)
          case ProductTerm(id, s)   => Writes(a => Json.obj(fieldLabel(id) -> s.writes(a)))
          case Record(base, iso)    => base.contramap(iso.reverseGet)
          case SeqSchema(elem)      => Writes(seq => JsArray(seq.map(elem.writes(_))))
          case IsoSchema(base, iso) => base.contramap(iso.reverseGet)
        }

      }

      def interpret = hyloNT(labelRecordFields, alg).compose(labellingSeed)

    }
}
