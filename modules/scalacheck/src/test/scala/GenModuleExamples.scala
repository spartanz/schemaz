import testz._
import org.scalacheck.rng.Seed
import scalaz.Scalaz._
import scalaz.schema._
import scalaz.schema.JsonSchema.{ JsonBool, JsonNull, JsonNumber, JsonString }
import scalaz.schema.scalacheck.GenModule
import scalaz.{ schema, ~> }

object GenModuleExamples {

  val jsonModule = new GenModule {
    type Prim[A]       = JsonSchema.Prim[A]
    type ProductTermId = String
    type SumTermId     = String
  }

  def tests[T](harness: Harness[T]): T = {
    import harness._
    import jsonModule._
    import org.scalacheck.Gen
    import org.scalacheck.Arbitrary._

    section("Generating Gens")(
      test("Convert Schema to Gen") { () =>
        val personSchema = record[Person](
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

        implicit val primToGenNT = new (Prim ~> Gen) {
          override def apply[A](prim: JsonSchema.Prim[A]): Gen[A] = prim match {
            case JsonString => arbitrary[String]
            case JsonNumber => arbitrary[BigDecimal]
            case JsonBool   => arbitrary[Boolean]
            case JsonNull   => arbitrary[Null]
          }
        }

        val personGen: Gen[Person] = personSchema.toGen

        val expectedUserGen: Gen[User] = for {
          active <- arbitrary[Boolean]
        } yield User(active)

        val expectedAdminGen: Gen[Admin] = for {
          rights <- Gen.listOf(arbitrary[String])
        } yield Admin(rights)

        val expectedPersonGen: Gen[Person] = for {
          name <- arbitrary[String]
          role <- Gen.option(Gen.oneOf(expectedUserGen, expectedAdminGen))
        } yield schema.Person(name, role)

        val genParameters = Gen.Parameters.default
        val genSeed       = Seed.random()

        // _.pureApply is not pure, it my throw Gen.RetrievalError
        assert(
          personGen.pureApply(genParameters, genSeed) == expectedPersonGen
            .pureApply(genParameters, genSeed)
        )
      }
    )
  }

}
