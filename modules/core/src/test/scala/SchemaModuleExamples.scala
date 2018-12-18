package scalaz

package schema

import monocle.Iso
import testz._
//import FreeChoice._ necessary for commented test
/*
object SchemaModuleExamples {

  def tests[T](harness: Harness[T]): T = {
    import harness._
    import scalaz.schema.Json.module._
    import scalaz.schema.Json.module.Schema._

    section("Manipulating Schemas")(
      //This test is currently broken because of the use of ==
      /*test("Building Schemas using the smart constructors") { () =>
        type PersonTuple = (Seq[Char], Option[Role])
        val personTupleSchema = iso[Person, PersonTuple](
          record(
            essentialField[Person, String](
              "name",
              prim(JsonSchema.JsonString),
            ) ::
            FPure(
              nonEssentialField[Person, Role](
                "role",
                union(
                  branch[Role, User](
                    "user",
                    record(
                      FPure(
                        essentialField[User,Boolean](
                          "active",
                          prim(JsonSchema.JsonBool),
                        )
                      )
                    )(
                      Person.userActive,
                      User.apply
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
                          Person.adminRights,
                          Admin.apply
                        )
                      )
                    )
                )(
                  Person.roleEither,
                  Person.eitherRole
                )
              )
            )
        )(
          Person.personProduct,
          Person.productPerson
        ),
          Person.personToTupleIso
        )

        val expected =
          IsoSchema[Person, (Seq[Char], Option[Role])](
            RecordSchema[Person, (String, Option[Role])](
              FAp(
                Term[ProductTermId, Person, String](
                  "name",
                  PrimSchema(JsonSchema.JsonString)
                ),
                FPure(
                  Term[ProductTermId, Person, Option[Role]](
                    "role",
                    OptionalSchema(
                      Union(
                        ChoiceBranch[Term[SumTermId, Role, ?], User, Admin](
                          Term[SumTermId, Role, User](
                            "user",
                            RecordSchema[User, Boolean](
                              FPure(
                                Term[ProductTermId, User, Boolean](
                                  "active",
                                  PrimSchema(JsonSchema.JsonBool)
                                )
                              ),
                              Person.userActive,
                              User.apply
                            )
                          ),
                          End[Term[SumTermId, Role, ?], Admin](
                            Term[SumTermId, Role, Admin](
                              "admin",
                              RecordSchema[Admin, List[String]](
                                FPure(
                                  Term[ProductTermId, Admin, List[String]](
                                    "rights",
                                    SeqSchema(PrimSchema(JsonSchema.JsonString))
                                  )
                                ),
                                Person.adminRights,
                                Admin.apply
                              )
                            )
                          )
                        ),
                        Person.roleEither,
                        Person.eitherRole
                      )
                    )
                  )
                )
              ),
              Person.personProduct,
              Person.productPerson
            ),
            Person.personToTupleIso
          )


        assert(personTupleSchema == expected)
      },*/

      /*
      test("imap on IsoSchema shouldn't add new layer") { () =>
        val adminToListIso  = Iso[Admin, List[String]](_.rights)(Admin.apply)
        def listToSeqIso[A] = Iso[List[A], Seq[A]](_.toSeq)(_.toList)

        val adminSchema = record(
          FPure(
            essentialField[Admin, List[String]](
              "rights",
              seq(prim(JsonSchema.JsonString))
            )
          )
        )(
          a => a.rights,
          rights => Admin(rights)
        )

        adminSchema.imap(adminToListIso).imap(listToSeqIso) match {
          case IsoSchema(base, _) => assert(base == adminSchema)
          case _                  => assert(false)
        }
      }
 */
    )
  }
}
 */
