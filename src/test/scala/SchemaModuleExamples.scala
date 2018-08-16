package scalaz

package schema

import testz._
import monocle._
import monocle.macros._

object SchemaModuleExamples {

  val jsonModule = new SchemaModule {
    type Prim[A]       = JsonSchema.Prim[A]
    type ProductTermId = String
    type SumTermId     = String
  }

  final case class Person(name: String, role: Option[Role])
  sealed trait Role
  final case class User(active: Boolean)       extends Role
  final case class Admin(rights: List[String]) extends Role

  object Person {
    val name = Getter[Person, String](_.name)

    def setRole(r1: Role): Person => Person = (p: Person) => p.copy(role = Some(r1))
    val role                                = monocle.Optional[Person, Role](_.role)(setRole(_))

    val user   = GenPrism[Role, User]
    val admin  = GenPrism[Role, Admin]
    val active = Getter[User, Boolean](_.active)
    val rights = Getter[Admin, List[String]](_.rights)

  }

  def tests[T](harness: Harness[T]): T = {
    import harness._
    import jsonModule._
    import jsonModule.Schema._

    section("Manipulating Schemas")(
      test("Building Schemas using the smart constructors") { () =>
        val personSchema = record[Person](
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
                  essentialField("active", prim(JsonSchema.JsonBool), Person.active, None)
                ),
                Person.user
              ),
              branch(
                "admin",
                record[Admin](
                  essentialField("rights", seq(prim(JsonSchema.JsonString)), Person.rights, None)
                ),
                Person.admin
              )
            ),
            Person.role
          )
        )
        val expected =
          RecordSchema[Person](
            NonEmptyList.nels(
              Field.Essential[Person, String](
                "name",
                PrimSchema(JsonSchema.JsonString),
                Person.name,
                None
              ),
              Field.NonEssential[Person, Role](
                "role",
                Union(
                  NonEmptyList.nels(
                    Branch[Role, User](
                      "user",
                      RecordSchema[User](
                        NonEmptyList.nels(
                          Field.Essential(
                            "active",
                            PrimSchema(JsonSchema.JsonBool),
                            Person.active,
                            None
                          )
                        )
                      ),
                      Person.user
                    ),
                    Branch[Role, Admin](
                      "admin",
                      RecordSchema[Admin](
                        NonEmptyList.nels(
                          Field.Essential(
                            "rights",
                            SeqSchema(PrimSchema(JsonSchema.JsonString)),
                            Person.rights,
                            None
                          )
                        )
                      ),
                      Person.admin
                    )
                  )
                ),
                Person.role
              )
            )
          )
        assert(personSchema == expected)
      }
    )
  }
}
