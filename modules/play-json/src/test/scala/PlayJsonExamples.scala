package scalaz

package schema

package play.json

import testz._
import _root_.play.api.libs.json._
import monocle.Iso
import org.scalacheck._, Prop._, Arbitrary._

object PlayJsonExamples {

  val jsonModule = new PlayJsonModule[JsonSchema.type] with scalacheck.GenModule[JsonSchema.type] {
    val R = JsonSchema

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
    import Schema._
    import jsonModule._
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
        val user = record(
          "active" -*>: prim(JsonSchema.JsonBool),
          Iso[Boolean, User](User.apply)(_.active)
        )

        val admin = record(
          "rights" -*>: seq(prim(JsonSchema.JsonString)),
          Iso[List[String], Admin](Admin.apply)(_.rights)
        )

        val person = record(
          "name" -*>: prim(JsonSchema.JsonString) :*:
            "role" -*>: optional(
            union(
              "user" -+>: user :+:
                "admin" -+>: admin,
              Iso[User \/ Admin, Role] {
                case -\/(u) => u
                case \/-(a) => a
              } {
                case u @ User(_)  => -\/(u)
                case a @ Admin(_) => \/-(a)
              }
            )
          ),
          Iso[(String, Option[Role]), Person]((Person.apply _).tupled)(p => (p.name, p.role))
        )

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
