package schemaz

package tests
import scalaz.{ Show, ~> }
import testz._
import generic._

object ShowExamples {

  val showModule = new TestModule with ShowModule[JsonSchema.type] {

    import JsonSchema._

    val primToShowNT = new (JsonSchema.Prim ~> Show) {

      def apply[A](fa: JsonSchema.Prim[A]): Show[A] =
        fa match {
          case JsonNumber => Show.showFromToString[BigDecimal]
          case JsonBool   => Show.showFromToString[Boolean]
          case JsonString => Show.shows[String](s => s""""$s"""")
          case JsonNull   => Show.shows[Unit](_ => "null")
        }
    }

    implicit val interpreter =
      Interpreter.cata(showAlgebra(primToShowNT, identity[String], identity[String]))

  }

  def comp(s1: String, s2: String): Boolean =
    s1.replaceAll("\\s+", "").trim.toLowerCase() == s2.replaceAll("\\s+", "").trim.toLowerCase()

  def tests[T](harness: Harness[T]): T = {
    import harness._
    import showModule._

    section("Generating Show Instances")(
      test("commons Show Instance") { () =>
        {
          val boss = Person("Alfred", None)

          val testCases: List[(Person, String)] = List(
            Person(null, None)                                          -> """(name = ("null"), role = ())""",
            boss                                                        -> """(name = ("Alfred"), role = ())""",
            Person("Alfred the Second", Some(User(true, boss)))         -> """(name = ("Alfred the Second"), role = (user = ((active = (true), boss = (( name = ("Alfred"), role = ()))))))""",
            Person("Alfred the Third", Some(Admin(List("sys", "dev")))) -> """(name = ("Alfred the Third"), role = (admin = (rights = (["sys","dev"]))))"""
          )

          val show = person.to[Show]

          testCases.foldLeft[Result](Succeed)(
            (res, testCase) =>
              (res, testCase) match {
                case (Succeed, (data, expected)) => {

                  val showS = show.shows(data)

                  if (comp(showS, expected)) Succeed
                  else Fail(List(Right(s"got '$showS' expected $expected")))

                }

                case (fail: testz.Fail, (data, expected)) => {
                  val showS = show.shows(data)

                  if (comp(showS, expected)) fail
                  else Fail(Right(s"got '$showS' expected $expected") :: fail.failures)

                }

              }
          )

        }
      }
    )
  }
}
