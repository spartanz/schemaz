package schemaz

package tests
import scalaz.~>
import testz._
import _root_.play.api.libs.json._
import org.scalacheck._, Prop._, Arbitrary._

object PlayJsonExamples {

  val module = new TestModule with play.json.PlayJsonModule[JsonSchema.type]
  with scalacheck.GenModule[JsonSchema.type] {

    implicit val primToGenNT = new (JsonSchema.Prim ~> Gen) {
      override def apply[A](prim: JsonSchema.Prim[A]): Gen[A] = prim match {
        case JsonSchema.JsonString => arbitrary[String]
        case JsonSchema.JsonNumber => arbitrary[BigDecimal]
        case JsonSchema.JsonBool   => arbitrary[Boolean]
        case JsonSchema.JsonNull   => arbitrary[Unit]
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
  }

  def tests[T](harness: Harness[T]): T = {
    import harness._
    import module._
    import JsonSchema._

    section("play-json codecs")(
      test("Cumbersome tuple nested representation") { () =>
        val triple = prim(JsonString) :*: prim(JsonBool) :*: prim(JsonString)

        val value = ("foo", (true, "bar"))

        assert(
          triple.to[Writes].writes(value) == Json
            .obj("_1" -> "foo", "_2" -> Json.obj("_1" -> true, "_2" -> "bar"))
        )
      },
      test("Derived `Reads` and `Writes` for a given schema should be symmetrical") { () =>
        implicit val personGen = Arbitrary(person.to[Gen])

        val reader = person.to[Reads]
        val writer = person.to[Writes]

        val prop = forAll { (pers: Person) =>
          reader.reads(writer.writes(pers)) == JsSuccess(pers)
        }
        val result = Test.check(Test.Parameters.default, prop)
        if (result.passed) Succeed else Fail(List(Right(result.status.toString)))

      }
    )
  }
}
