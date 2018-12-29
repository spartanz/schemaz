package scalaz

package schema

package scalacheck

import testz._
import org.scalacheck._, Prop._, rng.Seed
import monocle._

object GenModuleExamples {

  val jsonModule = new GenModule[JsonSchema.type] {}

  def tests[T](harness: Harness[T]): T = {
    import harness._
    import Schema._
    import jsonModule._
    import org.scalacheck.Gen
    import org.scalacheck.Arbitrary._

    section("Generating Gens")(
      test("Convert Schema to Gen") { () =>
        type PersonTuple = (Seq[Char], Option[Role])

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

        val personTupleSchema = iso[Person, PersonTuple](
          record(
            "name" -*>: prim(JsonSchema.JsonString) :*:
              "role" -*>: optional(
              role
            ),
            Iso[(String, Option[Role]), Person]((Person.apply _).tupled)(p => (p.name, p.role))
          ),
          Person.personToTupleIso
        )

        implicit val primToGenNT = new (JsonSchema.Prim ~> Gen) {
          override def apply[A](prim: JsonSchema.Prim[A]): Gen[A] = prim match {
            case JsonSchema.JsonString => arbitrary[String]
            case JsonSchema.JsonNumber => arbitrary[BigDecimal]
            case JsonSchema.JsonBool   => arbitrary[Boolean]
            case JsonSchema.JsonNull   => arbitrary[Null]
          }
        }

        val personGen: Gen[PersonTuple] = personTupleSchema.to[Gen]

        val prop = forAll {
          (seed1: Long, seed2: Long) =>
            val genParameters = Gen.Parameters.default
            val genSeed1      = Seed(seed1)
            val genSeed2      = Seed(if (seed1 != seed2) seed2 else seed2 + 1) // make sure both seeds are different

            val stream = Gen.infiniteStream(personGen)
            stream
              .pureApply(genParameters, genSeed1)
              .zip(stream.pureApply(genParameters, genSeed1))
              .take(100)
              .toList
              .forall(p => p._1 == p._2) &&
            stream
              .pureApply(genParameters, genSeed1)
              .zip(stream.pureApply(genParameters, genSeed2))
              .take(100)
              .toList
              .forall(p => p._1 != p._2)
        }

        val result = prop(Gen.Parameters.default)
        println(result)
        if (result.success) Succeed else Fail(List(Right(result.status.toString)))
      }
    )
  }
}
