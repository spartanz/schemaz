package scalaz

package schema

package tests

import scalacheck._

import testz._
import org.scalacheck._, Prop._, rng.Seed, Arbitrary._

trait PrimToGen {
  implicit val primToGenNT = new (JsonSchema.Prim ~> Gen) {
    override def apply[A](prim: JsonSchema.Prim[A]): Gen[A] = prim match {
      case JsonSchema.JsonString => arbitrary[String]
      case JsonSchema.JsonNumber => arbitrary[BigDecimal]
      case JsonSchema.JsonBool   => arbitrary[Boolean]
      case JsonSchema.JsonNull   => arbitrary[Unit]
    }
  }

}

object GenModuleExamples {

  def tests[T](harness: Harness[T]): T = {
    import harness._

    section("Generating Gens")(
      test("Convert Schema to Gen") { () =>
        val module = new TestModule with GenModule[JsonSchema.type] with PrimToGen {}
        import module._

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
        if (result.success) Succeed else Fail(List(Right(result.status.toString)))
      },
      test("Convert Schema to Gen with Generic Module") { () =>
        val module = new TestModule with GenericGenModule[JsonSchema.type] with PrimToGen {}
        import module._
        val personGen: Gen[PersonTuple] = personTupleSchema.to[Gen]

        val prop = forAll {
          (seed1: Long, seed2: Long) =>
            val genParameters = Gen.Parameters.default.withSize(10)
            val genSeed1      = Seed(seed1)
            val genSeed2      = Seed(if (seed1 != seed2) seed2 else seed2 + 1) // make sure both seeds are different

            val stream = Gen.infiniteStream(personGen)
            stream
              .pureApply(genParameters, genSeed1)
              .zip(stream.pureApply(genParameters, genSeed1))
              .take(10)
              .toList
              .forall(p => p._1 == p._2) &&
            stream
              .pureApply(genParameters, genSeed1)
              .zip(stream.pureApply(genParameters, genSeed2))
              .take(10)
              .toList
              .forall(p => p._1 != p._2)
        }

        val result = Test.check(Test.Parameters.default, prop)
        if (result.passed) Succeed else Fail(List(Right(result.status.toString)))
      }
    )
  }
}
