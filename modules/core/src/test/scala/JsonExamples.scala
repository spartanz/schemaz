package scalaz

package schema

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
        val schema = record(
          essentialField[Person, String](
            "name",
            prim(JsonSchema.JsonString)
          ) ::
            FPure(
              nonEssentialField[Person, Role](
                "role",
                union(
                  branch[Role, User](
                    "user",
                    record(
                      FPure(
                        essentialField[User, Boolean](
                          "active",
                          prim(JsonSchema.JsonBool)
                        )
                      )
                    )(
                      u => u.active,
                      active => User(active)
                    )
                  ) ::
                    End(
                      branch[Role, Admin](
                        "admin",
                        record(
                          FPure(
                            essentialField[Admin, List[String]](
                              "rights",
                              seq(prim(JsonSchema.JsonString))
                            )
                          )
                        )(
                          admin => admin.rights,
                          rights => Admin(rights)
                        )
                      )
                    )
                )(
                  {
                    case u: User  => -\/(u)
                    case a: Admin => \/-(a)
                  }, {
                    case -\/(user)  => user
                    case \/-(admin) => admin
                  }
                )
              )
            )
        )(
          p => (p.name, p.role),
          { case (name, role) => Person(name, role) }
        )

        type PersonTuple = (Seq[Char], Option[Role])
        val personTupleSchema = iso[Person, PersonTuple](schema, Person.personToTupleIso)

        val serializer: Person => JSON =
          jsonSerializer(module)(schema)(identity, identity)

        val isoSerializer: PersonTuple => JSON =
          jsonSerializer(module)(personTupleSchema)(
            identity,
            identity
          )

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
