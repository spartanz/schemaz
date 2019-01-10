package scalaz

package schema

import testz._
import monocle._

object JsonExamples {

  import Json._
  import Schema._

  def tests[T](harness: Harness[T]): T = {
    import harness._
    import JsonSchema.{ Prim => _, _ }

    val module = new JsonModule[JsonSchema.type] {
      override val R = JsonSchema
    }

    import module._

    def matchJsonStrings(a: String, b: String): Boolean =
      a.toLowerCase.replaceAll("\\s+", "") == b.toLowerCase.replaceAll("\\s+", "")

    section("JSON Schema Tests")(
      test("Case Class should Serialize using Schema") { () =>
        val role = union(
          "user" -+>: record(
            "active" -*>: prim(JsonSchema.JsonBool),
            Iso[Boolean, User](User.apply)(_.active)
          ) :+:
            "admin" -+>: record(
            "rights" -*>: seq(prim(JsonSchema.JsonString)),
            Iso[List[String], Admin](Admin.apply)(_.rights)
          ),
          Iso[User \/ Admin, Role] {
            case -\/(u) => u
            case \/-(a) => a
          } {
            case u @ User(_)  => -\/(u)
            case a @ Admin(_) => \/-(a)
          }
        )

        val schema = record(
          "name" -*>: prim(JsonSchema.JsonString) :*:
            "role" -*>: optional(
            role
          ),
          Iso[(String, Option[Role]), Person]((Person.apply _).tupled)(p => (p.name, p.role))
        )

        implicit val primToEncoderNT = new (JsonSchema.type#Prim ~> Encoder) {
          def apply[A](fa: JsonSchema.type#Prim[A]): Encoder[A] = { a =>
            fa match {
              case JsonNumber => a.toString
              case JsonBool   => a.toString
              case JsonString => s""""$a""""
              case JsonNull   => "null"
            }
          }
        }

        val serializer: Encoder[Person] = schema.to[Encoder]

//        type PersonTuple = (Seq[Char], Option[Role])
        val personTupleSchema = iso(schema, Person.personToTupleIso)

        val isoSerializer = personTupleSchema.to[Encoder]

        val testCases: List[(Person, String)] = List(
          Person(null, None)                                          -> """{"name":null}""",
          Person("Alfred", None)                                      -> """{"name":"Alfred"}""",
          Person("Alfred the Second", Some(User(true)))               -> """{"name":"Alfred the Second", "role": {"user": {"active":true}}}""",
          Person("Alfred the Third", Some(Admin(List("sys", "dev")))) -> """{"name":"Alfred the Third", "role": {"admin": {"rights": ["sys", "dev"]}}}"""
        )

        testCases.foldLeft[Result](Succeed)(
          (res, testCase) =>
            (res, testCase) match {
              case (Succeed, (data, expected)) => {
                val json    = serializer(data)
                val isoJson = isoSerializer(Person.personToTupleIso.reverse(data))

                val same    = matchJsonStrings(json, expected)
                val isoSame = matchJsonStrings(isoJson, expected)

                val res =
                  if (same) Succeed else Fail(List(Right(s"got $json expected $expected")))
                val isoRes =
                  if (isoSame) Succeed
                  else Fail(List(Right(s"got $isoJson expected $expected")))

                Result.combine(res, isoRes)
              }

              case (fail: testz.Fail, (data, expected)) => {
                val json    = serializer(data)
                val isoJson = isoSerializer(Person.personToTupleIso.reverse(data))
                val same    = matchJsonStrings(json, expected)
                val isoSame = matchJsonStrings(isoJson, expected)

                val res =
                  if (same) Succeed
                  else Fail(Right(s"got $json expected $expected") :: fail.failures)
                val isoRes =
                  if (isoSame) Succeed
                  else Fail(Right(s"got $isoJson expected $expected") :: fail.failures)

                Result.combine(res, isoRes)
              }

            }
        )

      }
    )
  }

}
