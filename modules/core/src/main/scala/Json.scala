package scalaz

package schema

import scalaz.Scalaz._
import scalaz.schema.JsonSchema._
import FreeChoice._

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

  /*
  I am not happy with a couple of thins. Namely with the requirement of a MonadError here. I think ApplicativeError should be enough and it shouldn't be needed in the first place.
  Only place where there's a possible error is in the Union Branches and it "shouldn't ever be the case" I think. There should always be exactly ONE union branch applicable.
   */
  def jsonSerializer[A](
    schemaModule: SchemaModule
  )(
    schema: schemaModule.Schema[A]
  )(
    productId: schemaModule.ProductTermId => module.ProductTermId,
    sumId: schemaModule.SumTermId => module.SumTermId
  )(
    implicit
    prims: ToJson[schemaModule.Prim]
  ): A => JSON = schema match {
    case schemaModule.Schema.PrimSchema(prim) => a => prims.serializer(prim)(a)
    case schemaModule.Schema.IsoSchema(base, iso) =>
      a => jsonSerializer(schemaModule)(base)(productId, sumId)(prims)(iso(a))
    case schemaModule.Schema.RecordSchema(fields) =>
      a =>
        fields
          .analyze[IList[JSON]](
            new (schemaModule.Schema.Field[A, ?] ~> λ[b => IList[JSON]]) {
              override def apply[B](
                fa: schemaModule.Schema.Field[A, B]
              ): IList[JSON] =
                fa match {
                  case schemaModule.Schema.Field.Essential(id, base, getter, _) =>
                    IList(s""""${productId(id)}": ${jsonSerializer(schemaModule)(base)(
                      productId,
                      sumId
                    )(prims)(getter.get(a))}""")
                  case schemaModule.Schema.Field.NonEssential(id, base, getter) =>
                    getter
                      .getOption(a)
                      .fold(
                        IList.apply[JSON]()
                      )(
                        b =>
                          IList(s""""${productId(id)}": ${jsonSerializer(schemaModule)(base)(
                            productId,
                            sumId
                          )(prims)(b)}""")
                      )
                }
            }
          )(
            new Monoid[IList[JSON]] {
              override def zero: IList[JSON] = IList.apply[JSON]()

              override def append(f1: IList[JSON], f2: => IList[JSON]): IList[JSON] = f2 |+| f1
            }
          )
          .toList
          .mkString("{", ",", "}")

    case schemaModule.Schema.SeqSchema(element) =>
      a =>
        a.map(jsonSerializer(schemaModule)(element)(productId, sumId)(prims))
          .mkString("[", ",", "]")
    case union: schemaModule.Schema.Union[_, ae] =>
      a => {
        val nt: ((schemaModule.Schema.Branch[A, ?]) ~> (? => JSON)) =
          new ((schemaModule.Schema.Branch[A, ?]) ~> (? => JSON)) {
            override def apply[X](branch: schemaModule.Schema.Branch[A, X]): X => JSON =
              (x: X) =>
                s"""{"${branch.id}": ${jsonSerializer(schemaModule)(branch.base)(productId, sumId)(
                  prims
                )(x)}}"""
          }

        val fun =
          FreeChoice.contravariantFold[schemaModule.Schema.Branch[A, ?], ? => JSON, ae](
            union.choices
          )(nt)
        fun(union.f(a))
      }
  }

}
