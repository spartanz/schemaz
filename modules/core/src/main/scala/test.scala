import scalaz._, schema._
import monocle._

import shapeless.syntax.singleton._

object module extends JsonModule[JsonSchema.type] with HasMigration[JsonSchema.type] {

//  import Transform._

  val R = JsonSchema

  final case class Person(name: String, role: Option[Role])
  sealed trait Role
  final case class User(active: Boolean, boss: Person) extends Role
  final case class Admin(rights: List[String])         extends Role

  trait Version2 {

    def user(pers: BareSchema[Person]) = record(
      "active".narrow -*>: prim(JsonSchema.JsonBool) :*: "boss".narrow -*>: self(pers),
      Iso[(Boolean, Person), User]((User.apply _).tupled)(u => (u.active, u.boss))
    )

    val admin = record(
      "rights".narrow -*>: seq(prim(JsonSchema.JsonString)),
      Iso[List[String], Admin](Admin.apply)(_.rights)
    )

    def role(pers: Schema_[Person]) = union(
      "user".narrow -+>: user(pers) :+:
        "admin".narrow -+>: admin,
      Iso[User \/ Admin, Role] {
        case -\/(u) => u
        case \/-(a) => a
      } {
        case u @ User(_, _) => -\/(u)
        case a @ Admin(_)   => \/-(a)
      }
    )

    def person(pers: Schema_[Person]) = record(
      "name".narrow -*>: prim(JsonSchema.JsonString) :*:
        "role".narrow -*>: optional(
        role(pers)
      ),
      Iso[(String, Option[Role]), Person]((Person.apply _).tupled)(p => (p.name, p.role))
    )

  }
}
