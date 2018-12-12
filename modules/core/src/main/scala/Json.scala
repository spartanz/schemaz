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

  /*def nt[ID, G[_], A](
    schemaModule: SchemaModule
  )(
    pure:JSON => G[JSON]
  )(
    implicit
    prims: ToJson[schemaModule.Prim]
  ): ((schemaModule.Schema.Term[ID, A, ?]) ~> (? => G[JSON])) = new ((schemaModule.Schema.Term[ID, A, ?]) ~> (? => G[JSON])) {
            //TODO: might be dangerzone because schemamodule termids might not be string.
    override def apply[X](term: schemaModule.Schema.Term[module.SumTermId, A, X]): X => G[JSON] = (x: X) =>
        pure(s"""{"${term.id}": ${jsonSerializer(schemaModule)(term.base)(prims)(x)}}""")
  }*/

  /*
  I am not happy with a couple of thins. Namely with the requirement of a MonadError here. I think ApplicativeError should be enough and it shouldn't be needed in the first place.
  Only place where there's a possible error is in the Union Branches and it "shouldn't ever be the case" I think. There should always be exactly ONE union branch applicable.
   */
  def jsonSerializer[A](
    schemaModule: SchemaModule
  )(
    schema: schemaModule.Schema[A]
  )(
    sumID: schemaModule.SumTermId => module.SumTermId,
    productID: schemaModule.ProductTermId => module.ProductTermId
  )(
    implicit
    prims: ToJson[schemaModule.Prim]
  ): A => JSON = {

    //Provokes BS Warning
    def nt[ID, IDM, G[_]](
      idf: ID => IDM
    )(pure: JSON => G[JSON]): ((schemaModule.Schema.Term[ID, A, ?]) ~> (? => G[JSON])) =
      new ((schemaModule.Schema.Term[ID, A, ?]) ~> (? => G[JSON])) {
        override def apply[X](term: schemaModule.Schema.Term[ID, A, X]): X => G[JSON] =
          (x: X) =>
            pure(s""""${idf(term.id)}": ${jsonSerializer(schemaModule)(term.base)(sumID, productID)(
              prims
            )(x)}""")
      }

    schema match {
      case schemaModule.Schema.OptionalSchema(base) =>
        a => a.fold("null")(x => jsonSerializer(schemaModule)(base)(sumID, productID)(prims)(x))
      case schemaModule.Schema.PrimSchema(prim) => a => prims.serializer(prim)(a)
      case schemaModule.Schema.IsoSchema(base, iso) =>
        a => jsonSerializer(schemaModule)(base)(sumID, productID)(prims)(iso(a))
      case rec: schemaModule.Schema.RecordSchema[_, ap] =>
        a => {
          val fun = FreeAp2
            .contravariantFold[
              schemaModule.Schema.Term[schemaModule.ProductTermId, A, ?],
              ? => IList[JSON],
              ap
            ](
              rec.fields
            )(
              nt[schemaModule.ProductTermId, module.ProductTermId, IList](
                productID
              )(
                Monad[IList].pure[JSON](_)
              )
            )
          fun(rec.f(a)).toList.mkString("{", ",", "}")
        }

      case schemaModule.Schema.SeqSchema(element) =>
        a =>
          a.map(jsonSerializer(schemaModule)(element)(sumID, productID)(prims))
            .mkString("[", ",", "]")
      case union: schemaModule.Schema.Union[_, ae] =>
        a => {
          val fun =
            FreeChoice.contravariantFold[
              schemaModule.Schema.Term[schemaModule.SumTermId, A, ?],
              ? => JSON,
              ae
            ](
              union.choices
            )(
              nt[schemaModule.SumTermId, module.SumTermId, Id](
                sumID
              )(
                identity[JSON](_)
              )
            )
          "{" + fun(union.f(a)) + "}"
        }
    }
  }

}
