package schemaz.migrations

import schemaz._

import scalaz.~>
import testz._
import org.scalacheck._, Prop._, Arbitrary._
import _root_.play.api.libs.json._
import shapeless.syntax.singleton._

object MigrationExamples {

  case class PersonV0(role: Option[Role])

  def tests[T](harness: Harness[T]): T = {
    import harness._

    val jsonModule = new TestModule with HasMigration[JsonSchema.type]
    with examples.JsonModule[JsonSchema.type] with playjson.PlayJsonModule[JsonSchema.type]
    with scalacheck.GenModule[JsonSchema.type] with scalacheck.PrimToGen {
      override val realisation = JsonSchema

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

    import jsonModule._
    val version0 = current.migrate[Person].change(_.addField("name".narrow, "John Doe"))

    val person = version0.lookup[Person]

    val personV0 = caseClass(
      "role" -*> optional(current.lookup[Role]),
      NIso[Option[Role], PersonV0](PersonV0.apply, p => p.role)
    )

    section("Migrating schema")(
      test("Adding a field with default value should be backward compatible") { () =>
        val oldWriter = personV0.to[Writes]

        val newReader = person.to[Reads]

        implicit val arbPersonV0 = Arbitrary(personV0.to[Gen])

        val prop = forAll { (pers: PersonV0) =>
          val res = newReader.reads(oldWriter.writes(pers))

          res.filter(_ == Person("John Doe", pers.role)).isSuccess
        }
        val result = Test.check(Test.Parameters.default, prop)
        if (result.passed) Succeed else Fail(List(Right(result.status.toString)))

      },
      test("Adding a field with default value should be forward compatible") { () =>
        val oldReader = personV0.to[Reads]

        val newWriter = person.to[Writes]

        implicit val arbPerson = Arbitrary(person.to[Gen])

        val prop = forAll { (pers: Person) =>
          val res = oldReader.reads(newWriter.writes(pers))

          res.filter(_ == PersonV0(pers.role)).isSuccess
        }
        val result = Test.check(Test.Parameters.default, prop)
        if (result.passed) Succeed else Fail(List(Right(result.status.toString)))

      }
    )
  }
}
