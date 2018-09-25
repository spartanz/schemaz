package scalaz

package schema

import scalaz.Scalaz._
import scalaz.schema.JsonSchema._

object Json {
  type JSON = String

  val module = new SchemaModule {
    type Prim[A]       = JsonSchema.Prim[A]
    type ProductTermId = String
    type SumTermId     = String
  }

  trait ToJson[S[_]] {
    def serializer: S ~> (? => JSON)
  }

  implicit val toJson: ToJson[module.Prim] = new ToJson[module.Prim] {
    override def serializer: module.Prim ~> (? => JSON) = new (module.Prim ~> (? => JSON)) {
      override def apply[A](fa: module.Prim[A]): A => JSON = fa match {
        case JsonString => a => s""""$a""""
        case JsonBool   => a => if (a) "true" else "false"
        case JsonNull   => _ => "null"
        case JsonNumber => a => a.shows
      }
    }
  }

  sealed trait ToJsonErrors
  final case class UnionBranchError[A, S](a: A, schema: S) extends ToJsonErrors

  /*
  I am not happy with a couple of thins. Namely with the requirement of a MonadError here. I think ApplicativeError should be enough and it shouldn't be needed in the first place.
  Only place where there's a possible error is in the Union Branches and it "shouldn't ever be the case" I think. There should always be exactly ONE union branch applicable.
   */
  def jsonSerializer[M[_, _], E, A](
    schemaModule: SchemaModule
  )(
    schema: schemaModule.Schema[A]
  )(
    productId: schemaModule.ProductTermId => module.ProductTermId,
    sumId: schemaModule.SumTermId => module.SumTermId
  )(
    pe: ToJsonErrors => M[E, JSON]
  )(
    implicit
    prims: ToJson[schemaModule.Prim],
    M: Applicative[M[E, ?]]
  ): A => M[E, JSON] = schema match {
    case schemaModule.Schema.PrimSchema(prim) => a => M.pure(prims.serializer(prim)(a))
    case schemaModule.Schema.IsoSchema(base, iso) =>
      a => jsonSerializer(schemaModule)(base)(productId, sumId)(pe)(prims, M)(iso(a))
    case schemaModule.Schema.RecordSchema(fields) =>
      a =>
        fields
          .analyze[M[E, IList[JSON]]](
            new (schemaModule.Schema.Field[A, ?] ~> λ[b => M[E, IList[JSON]]]) {
              override def apply[B](
                fa: schemaModule.Schema.Field[A, B]
              ): M[E, IList[JSON]] =
                fa match {
                  case schemaModule.Schema.Field.Essential(id, base, getter, _) =>
                    jsonSerializer(schemaModule)(base)(productId, sumId)(pe)(prims, M)(
                      getter.get(a)
                    ).map(
                      json => IList(s""""${productId(id)}": $json""")
                    )
                  case schemaModule.Schema.Field.NonEssential(id, base, getter) =>
                    getter
                      .getOption(a)
                      .fold(
                        M.pure(IList.apply[JSON]())
                      )(
                        b =>
                          jsonSerializer(schemaModule)(base)(productId, sumId)(pe)(prims, M)(b).map(
                            json => IList(s""""${productId(id)}": $json""")
                          )
                      )
                }
            }
          )(
            new Monoid[M[E, IList[JSON]]] {
              override def zero: M[E, IList[JSON]] = M.pure(IList.apply[JSON]())

              override def append(f1: M[E, IList[JSON]], f2: => M[E, IList[JSON]])
                : M[E, IList[JSON]] = M.apply2(f1, f2)((x, y) => y |+| x)
            }
          )
          .map(
            _.toList
              .mkString("{", ",", "}")
          )

    case schemaModule.Schema.SeqSchema(element) =>
      a =>
        Traverse[List]
          .traverse(a)(jsonSerializer(schemaModule)(element)(productId, sumId)(pe)(prims, M))
          .map(_.mkString("[", ",", "]"))
    case s @ schemaModule.Schema.Union(terms) =>
      a => {
        terms
          .foldLeft(Option.empty[M[E, JSON]])(
            (opt, branch) =>
              (opt, branch) match {
                case (x @ Some(_), _) => x
                case (None, b: schemaModule.Schema.Branch[A, t]) =>
                  b.prism
                    .getOption(a)
                    .map(
                      value =>
                        jsonSerializer(schemaModule)(b.base)(productId, sumId)(pe)(prims, M)(value)
                          .map(
                            json => s"""{"${sumId(branch.id)}":$json}"""
                          )
                    )
              }
          )
          .fold(
            pe(UnionBranchError[A, schemaModule.Schema.Union[A]](a, s))
          )(
            identity
          )

      }
  }

}
