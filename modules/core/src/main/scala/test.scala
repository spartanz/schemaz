import schemaz._
import monocle._

import scalaz._

import shapeless.syntax.singleton._

object module
    extends JsonModule[JsonSchema.type]
    with HasMigration[JsonSchema.type]
    with Versioning[JsonSchema.type] {

//  import Transform._

  val R = JsonSchema

  implicit val primToEncoderNT = new (JsonSchema.Prim ~> Json.Encoder) {

    def apply[A](fa: JsonSchema.Prim[A]): Json.Encoder[A] = { a =>
      fa match {
        case JsonSchema.JsonNumber => a.toString
        case JsonSchema.JsonBool   => a.toString
        case JsonSchema.JsonString => s""""$a""""
        case JsonSchema.JsonNull   => "null"
      }
    }
  }

  final case class Person(name: String, role: Option[Role])
  sealed trait Role
  final case class User(active: Boolean, username: String)       extends Role
  final case class Admin(username: String, rights: List[String]) extends Role

  val burns =
    Person(
      "Montgommery Burns",
      Some(Admin(username = "releaseTheHounds", List("nuclear plant owner", "evil masternind")))
    )
  val homer = Person("Homer Simpson", Some(User(active = true, username = "SpiderPig")))

  val version2 = Current
    .schema(
      record(
        "active".narrow -*>: prim(JsonSchema.JsonBool) :*: "username".narrow -*>: prim(
          JsonSchema.JsonString
        ),
        Iso[(Boolean, String), User]((User.apply _).tupled)(u => (u.active, u.username))
      )
    )
    .schema(
      record(
        "username".narrow -*>: prim(JsonSchema.JsonString) :*: "rights".narrow -*>: seq(
          prim(JsonSchema.JsonString)
        ),
        Iso[(String, List[String]), Admin]((Admin.apply _).tupled)(a => (a.username, a.rights))
      )
    )
    .schema(
      (
        admin: Schema[Admin],
        user: Schema[User]
      ) =>
        union(
          "user".narrow -+>: user :+:
            "admin".narrow -+>: admin,
          Iso[User \/ Admin, Role] {
            case -\/(u) => u
            case \/-(a) => a
          } {
            case u @ User(_, _)  => -\/(u)
            case a @ Admin(_, _) => \/-(a)
          }
        )
    )
    .schema(
      (role: Schema[Role]) =>
        record(
          "name".narrow -*>: prim(JsonSchema.JsonString) :*:
            "role".narrow -*>: optional(role),
          Iso[(String, Option[Role]), Person]((Person.apply _).tupled)(p => (p.name, p.role))
        )
    )

  val version1 =
    version2
      .migrate[User]
      .change(_.addField("active".narrow, false))

  val version0 =
    version1
      .migrate[User]
      .change(_.addField("username".narrow, "John Doe"))
      .migrate[Admin]
      .change(_.addField("username".narrow, "John Doe"))

}
