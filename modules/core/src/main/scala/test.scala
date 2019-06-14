import scalaz._, schema._
import monocle._

//import shapeless._

import shapeless.syntax.singleton._

object module
    extends JsonModule[JsonSchema.type]
    with HasMigration[JsonSchema.type]
    with Migrations[JsonSchema.type] {

//  import Transform._

  val R = JsonSchema

  implicit val primToEncoderNT = new (JsonSchema.Prim ~> Json.EncoderA) {

    def apply[A](fa: JsonSchema.Prim[A]): Json.EncoderA[A] = { a =>
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

  def mkUser[Re] =
    (_: Registry[Re]) =>
      record(
        "active".narrow -*>: prim(JsonSchema.JsonBool) :*: "username".narrow -*>: prim(
          JsonSchema.JsonString
        ),
        Iso[(Boolean, String), User]((User.apply _).tupled)(u => (u.active, u.username))
      )

  def mkAdmin[Re] =
    (_: Registry[Re]) =>
      record(
        "username".narrow -*>: prim(JsonSchema.JsonString) :*: "rights".narrow -*>: seq(
          prim(JsonSchema.JsonString)
        ),
        Iso[(String, List[String]), Admin]((Admin.apply _).tupled)(a => (a.username, a.rights))
      )

  def mkRole[Re: Lookup[?, User]: Lookup[?, Admin]] =
    (r: Registry[Re]) =>
      union(
        "user".narrow -+>: r.lookup[User] :+:
          "admin".narrow -+>: r.lookup[Admin],
        Iso[User \/ Admin, Role] {
          case -\/(u) => u
          case \/-(a) => a
        } {
          case u @ User(_, _)  => -\/(u)
          case a @ Admin(_, _) => \/-(a)
        }
      )

  def mkPerson[Re: Lookup[?, Role]] =
    (reg: Registry[Re]) =>
      record(
        "name".narrow -*>: prim(JsonSchema.JsonString) :*:
          "role".narrow -*>: optional(
          reg.lookup[Role]
        ),
        Iso[(String, Option[Role]), Person]((Person.apply _).tupled)(p => (p.name, p.role))
      )

  val version2 = CtrRegistry.empty
    .addEntry(mkUser)
    .addEntry(mkAdmin)
    .addEntry(mkRole)
    .addEntry(mkPerson)

  val version2Reg = Registry.build(version2)

  def userV1[Re] =
    (r: Registry[Re]) => mkUser(r).addField("active".narrow, false)

  def adminV1[Re] = (r: Registry[Re]) => mkAdmin(r).addField("username".narrow, "John Doe")

  val version1 = version2
    .migrate[User]
    .apply(userV1)
    .migrate[Admin]
    .apply(adminV1)

  val version1Reg = Registry.build(version1)

  def userV0[Re] = (r: Registry[Re]) => userV1(r).addField("username".narrow, "John Doe")

  val version0 = version1.migrate[User].apply(userV0)

  val version0Reg = Registry.build(version0)

}
