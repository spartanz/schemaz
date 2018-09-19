package scalaz

package schema


import testz._

object JsonExamples {

  def tests[T](harness: Harness[T]): T = {
    import harness._
    import scalaz.schema.Json.module._
    import scalaz.schema.Json.module.Schema._

    section("JSON Schema Tests")(
      test("Case Class should Serialize using Schema") { () =>


      }
    )
  }

}
