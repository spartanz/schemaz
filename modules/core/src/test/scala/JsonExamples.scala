package scalaz

package schema

import scalaz.Scalaz._
import testz._

object JsonExamples {

  def tests[T](harness: Harness[T]): T = {
    import harness._
    import scalaz.schema.Json._
    import scalaz.schema.Json.module._

    def matchJsonStrings(a: String, b: String): Boolean =
      a.toLowerCase.replaceAll("\\s+", "") == b.toLowerCase.replaceAll("\\s+", "")

    section("JSON Schema Tests")(
      test("Case Class should Serialize using Schema") { () =>
        val schema = record[Person](
          ^(
            essentialField[Person, String](
              "name",
              prim(JsonSchema.JsonString),
              Person.name,
              None
            ),
            nonEssentialField[Person, Role](
              "role",
              union[Role](
                branch(
                  "user",
                  record[User](
                    essentialField(
                      "active",
                      prim(JsonSchema.JsonBool),
                      Person.active,
                      None
                    ).map(User.apply)
                  ),
                  Person.user
                ),
                branch(
                  "admin",
                  record[Admin](
                    essentialField(
                      "rights",
                      seq(prim(JsonSchema.JsonString)),
                      Person.rights,
                      None
                    ).map(Admin.apply)
                  ),
                  Person.admin
                )
              ),
              Person.role
            )
          )(Person.apply)
        )

        type PersonTuple = (Seq[Char], Option[Role])
        val personTupleSchema = iso[Person, PersonTuple](schema, Person.personToTupleIso)

        val serializer: Person => Either[ToJsonErrors, JSON] =
          jsonSerializer[Either[ToJsonErrors, ?], Person](schema)

        val isoSerializer: PersonTuple => Either[ToJsonErrors, JSON] =
          jsonSerializer[Either[ToJsonErrors, ?], PersonTuple](personTupleSchema)

        val testCases: List[(Person, String)] = List(
          Person("Alfred", None)                                      -> """{"name":"Alfred"}""",
          Person("Alfred the Second", Some(User(true)))               -> """{"name":"Alfred the Second", "role": {"user": {"active":true}}}""",
          Person("Alfred the Third", Some(Admin(List("sys", "dev")))) -> """{"name":"Alfred the Third", "role": {"admin": {"rights": ["sys", "dev"]}}}"""
        )

        testCases.foldLeft[Result](Succeed)(
          (res, testCase) =>
            (res, testCase) match {
              case (Succeed, (data, expected)) =>
                ^(serializer(data), isoSerializer(Person.personToTupleIso.reverse(data)))(
                  (json, isoJson) => {
                    val same    = matchJsonStrings(json, expected)
                    val isoSame = matchJsonStrings(isoJson, expected)

                    val res =
                      if (same) Succeed else Fail(List(Right(s"got $json expected $expected")))
                    val isoRes =
                      if (isoSame) Succeed
                      else Fail(List(Right(s"got $isoJson expected $expected")))

                    Result.combine(res, isoRes)
                  }
                ).fold(
                  {
                    case Json.UnionBranchError() =>
                      Fail(
                        List(Right("Union Branch Error (No Union Branch resulted in a Value)"))
                      )
                  },
                  identity
                )
              case (fail: testz.Fail, (data, expected)) =>
                ^(serializer(data), isoSerializer(Person.personToTupleIso.reverse(data)))(
                  (json, isoJson) => {
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
                ).fold(
                  {
                    case Json.UnionBranchError() =>
                      Fail(
                        Right("Union Branch Error (No Union Branch resulted in a Value)") :: fail.failures
                      )
                  },
                  identity
                )
            }
        )

      }
    )
  }

}
