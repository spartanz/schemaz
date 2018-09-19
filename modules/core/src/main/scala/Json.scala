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
        case JsonNull   => _ => null
        case JsonNumber => a => a.shows
      }
    }
  }

  sealed trait ToJsonErrors
  final case class UnionBranchError() extends ToJsonErrors

  def jsonSerializer[M[_], A](
    schema: module.Schema[A]
  )(
    implicit
    prims: ToJson[module.Prim],
    M: MonadError[M, ToJsonErrors]
  ): A => M[JSON] = schema match {
    case module.Schema.PrimSchema(prim) => a => M.pure(prims.serializer(prim)(a))
    case module.Schema.IsoSchema(_, _)  => ???
    case module.Schema.RecordSchema(fields) =>
      a =>
        M.pure(
          fields
            .foldMap[λ[b => State[DList[JSON], Const[Unit, b]]]](
              new (module.Schema.Field[A, ?] ~> λ[b => State[DList[JSON], Const[Unit, b]]]) {
                override def apply[B](
                  fa: module.Schema.Field[A, B]
                ): State[DList[String], Const[Unit, B]] =
                  fa match {
                    case module.Schema.Field.Essential(id, base, getter, _) =>
                      State.apply(
                        flds => {
                          val b = getter.get(a)
                          (
                            flds :+ (id + ":" + jsonSerializer(base)(prims, M)(b)),
                            Const.apply(())
                          )
                        }
                      )
                    case module.Schema.Field.NonEssential(id, base, getter) =>
                      getter
                        .getOption(a)
                        .fold(
                          State.apply[DList[String], Const[Unit, B]](flds => ???)
                        )(
                          b =>
                            State.apply(
                              flds =>
                                (
                                  flds :+ (id + ":" + jsonSerializer(base)(prims, M)(b)),
                                  Const.apply(())
                                )
                            )
                        )
                  }
              }
            )(
              Applicative[State[DList[JSON], ?]].compose(Applicative[Const[Unit, ?]])
            )
            .run(DList.apply[String]())
            .toList
            .mkString("{", ",", "}")
        )
    case module.Schema.SeqSchema(element) =>
      a =>
        Traverse[List].traverse(a)(jsonSerializer(element)(prims, M)).map(_.mkString("[", ",", "]"))
    case module.Schema.Union(terms) =>
      a => {
        terms
          .foldLeft(Option.empty[JSON])(
            (opt, branch) =>
              (opt, branch) match {
                case (x @ Some(_), _) => x
                case (None, b: module.Schema.Branch[A, t]) =>
                  b.prism
                    .getOption(a)
                    .map(value => jsonSerializer(b.base)(prims, M)(value))
                    .map(json => s"""{"${branch.id}":$json}""")
              }
          )
          .fold(
            M.raiseError[JSON](UnionBranchError())
          )(
            json => M.pure(json)
          )

      }
  }

}
