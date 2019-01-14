package scalaz

package schema

package generic

import testz._
import monocle._

object ShowExamples {

  val showModule = new ShowModule[JsonSchema.type] {
    val R = JsonSchema
  }

  def tests[T](harness: Harness[T]): T = {
    import harness._
    import Schema._
    import showModule._
    import JsonSchema._

    section("Generating Show Instances")(
      test("commons Show Instance") { () =>
        {
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

          val primToShowNT = new (showModule.R.Prim ~> Show) {
            def apply[A](fa: showModule.R.Prim[A]): Show[A] =
              fa match {
                case JsonNumber => Show.showFromToString[BigDecimal]
                case JsonBool   => Show.showFromToString[Boolean]
                case JsonString => Show.shows[String](s => s""""$s"""")
                case JsonNull   => Show.shows[Null](_ => "null")
              }
          }

          implicit val alg = showAlgebra(primToShowNT, identity[String], identity[String])

          val testCases: List[(Person, String)] = List(
            Person(null, None)                                          -> """(name = ("null"), role = (()))""",
            Person("Alfred", None)                                      -> """(name = ("Alfred"), role = (()))""",
            Person("Alfred the Second", Some(User(true)))               -> """(name = ("Alfred the Second"), role = (user = (active = (true))))""",
            Person("Alfred the Third", Some(Admin(List("sys", "dev")))) -> """(name = ("Alfred the Third"), role = (admin = (rights = (["sys","dev"]))))"""
          )

          val show = schema.to[Show]

          testCases.foldLeft[Result](Succeed)(
            (res, testCase) =>
              (res, testCase) match {
                case (Succeed, (data, expected)) => {

                  val showS = show.shows(data)

                  if (showS == expected) Succeed
                  else Fail(List(Right(s"got $showS expected $expected")))

                }

                case (fail: testz.Fail, (data, expected)) => {
                  val showS = show.shows(data)

                  if (showS == expected) fail
                  else Fail(Right(s"got $showS expected $expected") :: fail.failures)

                }

              }
          )

        }
      }
    )
  }
}
