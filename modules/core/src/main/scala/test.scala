import schemaz._
import monocle._

import scalaz._

import shapeless.syntax.singleton._

object module
    extends JsonModule[JsonSchema.type]
    with HasMigration[JsonSchema.type]
    with Migrations[JsonSchema.type] {

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

  val mkUser =
    (_: Unit) =>
      record(
        "active".narrow -*>: prim(JsonSchema.JsonBool) :*: "username".narrow -*>: prim(
          JsonSchema.JsonString
        ),
        Iso[(Boolean, String), User]((User.apply _).tupled)(u => (u.active, u.username))
      )

  val mkAdmin =
    (_: Unit) =>
      record(
        "username".narrow -*>: prim(JsonSchema.JsonString) :*: "rights".narrow -*>: seq(
          prim(JsonSchema.JsonString)
        ),
        Iso[(String, List[String]), Admin]((Admin.apply _).tupled)(a => (a.username, a.rights))
      )

  val mkRole =
    (admin: Schema[Admin], user: Schema[User]) =>
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

  val mkPerson =
    (role: Schema[Role]) =>
      record(
        "name".narrow -*>: prim(JsonSchema.JsonString) :*:
          "role".narrow -*>: optional(
          role
        ),
        Iso[(String, Option[Role]), Person]((Person.apply _).tupled)(p => (p.name, p.role))
      )

  val version2 = CtrRegistry.empty
    .addEntry(mkUser)
    .addEntry(mkAdmin)
    .addEntry(mkRole.tupled)
    .addEntry(mkPerson)

  val version2Reg = Registry.build(version2)

  val version1 = version2.replace(
    version2
      .migrate[User]
      .post(_.addField("active".narrow, false))
  )

  val version1Reg = Registry.build(version1)

  val version0 = version1.replace(
    version1
      .migrate[User]
      .post(_.addField("username".narrow, "John Doe"))
  )

  val version0Reg = Registry.build(version0)

}
