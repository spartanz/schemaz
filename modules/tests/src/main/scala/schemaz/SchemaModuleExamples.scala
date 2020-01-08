package schemaz

import testz._

object SchemaModuleExamples {

  def tests[T](harness: Harness[T]): T = {
    import harness._

    val jsonModule = new examples.JsonModule[JsonSchema.type] {
      override val R = JsonSchema
    }

    import jsonModule._

    section("Manipulating Schemas")(
      test("imap on IsoSchema shouldn't add new layer") { () =>
        //val adminToListIso  = NIso[Admin, List[String]](_.rights, Admin.apply)
        //def listToSeqIso[A] = NIso[List[A], Seq[A]](_.toSeq, _.toList)

        val adminRecord = "rights" -*> seq(prim(JsonSchema.JsonString))

        val adminSchema = caseClass(
          adminRecord,
          NIso[List[String], Admin](Admin.apply, _.rights)
        )

        assert(adminSchema.schema == record(adminRecord).schema)
      }
    )
  }
}
