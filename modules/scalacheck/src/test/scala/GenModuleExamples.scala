package scalaz

package schema

package scalacheck

import testz._
import org.scalacheck._, Prop._, rng.Seed
import monocle._

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
//        type PersonTuple = (Seq[Char], Option[Role])

        val user = record(
          "active" -*>: prim(JsonSchema.JsonBool),
          Iso[Boolean, User](User.apply)(_.active)
        )

        val admin = record(
          "rights" -*>: seq(prim(JsonSchema.JsonString)),
          Iso[List[String], Admin](Admin.apply)(_.rights)
        )

        val role = union(
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

        val personTupleSchema = //iso[Person, PersonTuple](
          record(
            "name" -*>: prim(JsonSchema.JsonString) :*:
              "role" -*>: optional(
              role
            ),
            Iso[(String, Option[Role]), Person]((Person.apply _).tupled)(p => (p.name, p.role))
            //),
            //Person.personToTupleIso
          )

        implicit val primToGenNT = new (Prim ~> Gen) {
          override def apply[A](prim: JsonSchema.Prim[A]): Gen[A] = prim match {
            case JsonSchema.JsonString => arbitrary[String]
            case JsonSchema.JsonNumber => arbitrary[BigDecimal]
            case JsonSchema.JsonBool   => arbitrary[Boolean]
            case JsonSchema.JsonNull   => arbitrary[Null]
          }
        }

        val personGen: Gen[Person] = personTupleSchema.toGen

        val expectedUserGen: Gen[User] = for {
          active <- arbitrary[Boolean]
        } yield User(active)

        val expectedAdminGen: Gen[Admin] = for {
          rights <- Gen.listOf(arbitrary[String])
        } yield Admin(rights)

        val expectedPersonTupleGen: Gen[Person] = for {
          role <- Gen.option(Gen.oneOf[Role](expectedUserGen, expectedAdminGen))
          name <- arbitrary[String].map(identity)
        } yield Person(name, role)

        val prop = forAll {
          (seed: Long) =>
            val genParameters = Gen.Parameters.default
            val genSeed       = Seed(seed)

            // _.pureApply is not pure, it my throw Gen.RetrievalError

            val actual =
              personGen.pureApply(genParameters, genSeed)
            val expected =
              expectedPersonTupleGen
                .pureApply(genParameters, genSeed)

            /*s"""name: ${actual.name == expected.name}
role: ${actual.role == expected.role}""" |: */
            actual == expected
        }

        val result = prop(Gen.Parameters.default)
        println(result)
        if (result.success) Succeed else Fail(List(Right(result.status.toString)))
      }
    )
  }
}
