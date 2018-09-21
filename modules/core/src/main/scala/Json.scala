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
    schema: module.Schema[A]
  )(
    implicit
    prims: ToJson[module.Prim],
    M: MonadError[M, ToJsonErrors]
  ): A => M[JSON] = schema match {
    case module.Schema.PrimSchema(prim)     => a => M.pure(prims.serializer(prim)(a))
    case module.Schema.IsoSchema(base, iso) => a => jsonSerializer(base)(prims, M)(iso(a))
    case module.Schema.RecordSchema(fields) =>
      a =>
        fields
          .foldMap[λ[b => StateT[M, IList[JSON], Const[Unit, b]]]](
            new (module.Schema.Field[A, ?] ~> λ[b => StateT[M, IList[JSON], Const[Unit, b]]]) {
              override def apply[B](
                fa: module.Schema.Field[A, B]
              ): StateT[M, IList[String], Const[Unit, B]] =
                fa match {
                  case module.Schema.Field.Essential(id, base, getter, _) =>
                    StateT.apply[M, IList[String], Const[Unit, B]](
                      flds => {
                        val b = getter.get(a)
                        jsonSerializer(base)(prims, M)(b).map(
                          json =>
                            (
                              (s""""$id": $json""") +: flds,
                              Const.apply(())
                            )
                        )

                      }
                    )
                  case module.Schema.Field.NonEssential(id, base, getter) =>
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
                              jsonSerializer(base)(prims, M)(b).map(
                                json =>
                                  (
                                    (s""""$id": $json""") +: flds,
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

    case module.Schema.SeqSchema(element) =>
      a =>
        Traverse[List].traverse(a)(jsonSerializer(element)(prims, M)).map(_.mkString("[", ",", "]"))
    case module.Schema.Union(terms) =>
      a => {
        terms
          .foldLeft(Option.empty[M[JSON]])(
            (opt, branch) =>
              (opt, branch) match {
                case (x @ Some(_), _) => x
                case (None, b: module.Schema.Branch[A, t]) =>
                  b.prism
                    .getOption(a)
                    .map(
                      value =>
                        jsonSerializer(b.base)(prims, M)(value).map(
                          json => s"""{"${branch.id}":$json}"""
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
