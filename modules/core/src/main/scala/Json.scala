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
  final case class UnionBranchError() extends ToJsonErrors

  /*
  I am not happy with a couple of thins. Namely with the requirement of a MonadError here. I think ApplicativeError should be enough and it shouldn't be needed in the first place.
  Only place where there's a possible error is in the Union Branches and it "shouldn't ever be the case" I think. There should always be exactly ONE union branch applicable.
   */
  def jsonSerializer[M[_], A](
    schemaModule: SchemaModule
  )(
    schema: schemaModule.Schema[A]
  )(
    productId: schemaModule.ProductTermId => module.ProductTermId,
    sumId: schemaModule.SumTermId => module.SumTermId
  )(
    implicit
    prims: ToJson[schemaModule.Prim],
    M: MonadError[M, ToJsonErrors]
  ): A => M[JSON] = schema match {
    case schemaModule.Schema.PrimSchema(prim) => a => M.pure(prims.serializer(prim)(a))
    case schemaModule.Schema.IsoSchema(base, iso) =>
      a => jsonSerializer(schemaModule)(base)(productId, sumId)(prims, M)(iso(a))
    case schemaModule.Schema.RecordSchema(fields) =>
      a =>
        fields
          .foldMap[λ[b => StateT[M, IList[JSON], Const[Unit, b]]]](
            new (schemaModule.Schema.Field[A, ?] ~> λ[b => StateT[M, IList[JSON], Const[Unit, b]]]) {
              override def apply[B](
                fa: schemaModule.Schema.Field[A, B]
              ): StateT[M, IList[String], Const[Unit, B]] =
                fa match {
                  case schemaModule.Schema.Field.Essential(id, base, getter, _) =>
                    StateT.apply[M, IList[String], Const[Unit, B]](
                      flds => {
                        val b = getter.get(a)
                        jsonSerializer(schemaModule)(base)(productId, sumId)(prims, M)(b).map(
                          json =>
                            (
                              (s""""${productId(id)}": $json""") +: flds,
                              Const.apply(())
                            )
                        )

                      }
                    )
                  case schemaModule.Schema.Field.NonEssential(id, base, getter) =>
                    getter
                      .getOption(a)
                      .fold(
                        StateT
                          .apply[M, IList[String], Const[Unit, B]](
                            flds => M.pure((flds, Const.apply(())))
                          )
                      )(
                        b =>
                          StateT.apply(
                            flds =>
                              jsonSerializer(schemaModule)(base)(productId, sumId)(prims, M)(b).map(
                                json =>
                                  (
                                    (s""""${productId(id)}": $json""") +: flds,
                                    Const.apply(())
                                  )
                              )
                          )
                      )
                }
            }
          )(
            Applicative[StateT[M, IList[JSON], ?]].compose(Applicative[Const[Unit, ?]])
          )
          .run(IList.apply[String]())
          .map(_._1)
          .map(
            _.toList
              .mkString("{", ",", "}")
          )

    case schemaModule.Schema.SeqSchema(element) =>
      a =>
        Traverse[List]
          .traverse(a)(jsonSerializer(schemaModule)(element)(productId, sumId)(prims, M))
          .map(_.mkString("[", ",", "]"))
    case schemaModule.Schema.Union(terms) =>
      a => {
        terms
          .foldLeft(Option.empty[M[JSON]])(
            (opt, branch) =>
              (opt, branch) match {
                case (x @ Some(_), _) => x
                case (None, b: schemaModule.Schema.Branch[A, t]) =>
                  b.prism
                    .getOption(a)
                    .map(
                      value =>
                        jsonSerializer(schemaModule)(b.base)(productId, sumId)(prims, M)(value).map(
                          json => s"""{"${sumId(branch.id)}":$json}"""
                        )
                    )
              }
          )
          .fold(
            M.raiseError[JSON](UnionBranchError())
          )(
            identity
          )

      }
  }

}
