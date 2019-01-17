package scalaz

package schema

package tests

import monocle.Iso

final case class Person(name: String, role: Option[Role])
sealed trait Role
final case class User(active: Boolean)       extends Role
final case class Admin(rights: List[String]) extends Role

object Person {

  private def f(p: Person): (Seq[Char], Option[Role]) = (p.name.toSeq, p.role)
  private def g(t: (Seq[Char], Option[Role])): Person = Person(t._1.mkString, t._2)

  val personToTupleIso = Iso[Person, (Seq[Char], Option[Role])](f)(g)
}

trait TestModule extends SchemaModule[JsonSchema.type] {
  val R = JsonSchema

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

  val person = record(
    "name" -*>: prim(JsonSchema.JsonString) :*:
      "role" -*>: optional(
      role
    ),
    Iso[(String, Option[Role]), Person]((Person.apply _).tupled)(p => (p.name, p.role))
  )

  val personTupleSchema = iso[Person, PersonTuple](
    person,
    Person.personToTupleIso
  )
}
