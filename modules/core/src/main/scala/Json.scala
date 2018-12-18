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

  type Encoder[A] = A => JSON

  def representation(primsNt: Prim ~> Encoder) = new module.Schema.Representation[Encoder] {

    val prims = primsNt

    val handleRecord = new (Encoder ~> Encoder) {
      def apply[A](fa: Encoder[A]): Encoder[A] = fa.andThen("{" + _ + "}")
    }

    val handleUnion = handleRecord

    def labelField(label: module.ProductTermId) = new (Encoder ~> Encoder) {
      def apply[A](fa: Encoder[A]): Encoder[A] = fa.andThen(s""""$label":""" + _)
    }

    def labelBranch(label: module.SumTermId) = new (Encoder ~> Encoder) {
      def apply[A](fa: Encoder[A]): Encoder[A] = fa.andThen(s""""$label":""" + _)
    }

    def handleList[A]: Encoder[A] => Encoder[List[A]] = {
      encA =>
        { list =>
          list.map(encA).mkString("[", ",", "]")

        }

    }

    def zero: Encoder[Unit] = (_ => "null")

  }

  implicit val encoderDecidable = functionDecidable(new Monoid[String] {
    def zero                            = ""
    def append(a: String, b: => String) = s"$a, $b"
  })
}
