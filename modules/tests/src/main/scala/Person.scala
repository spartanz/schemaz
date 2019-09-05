package schemaz

package tests
import scalaz.{ -\/, \/, \/- }
import monocle.Iso

import shapeless.syntax.singleton._

final case class Person(name: String, role: Option[Role])
sealed trait Role
final case class User(active: Boolean)       extends Role
final case class Admin(rights: List[String]) extends Role

object Person {

  private def f(p: Person): (Seq[Char], Option[Role]) = (p.name.toSeq, p.role)
  private def g(t: (Seq[Char], Option[Role])): Person = Person(t._1.mkString, t._2)

  val personToTupleIso = Iso[Person, (Seq[Char], Option[Role])](f)(g)
}

trait TestModule extends SchemaModule[JsonSchema.type] with Versioning[JsonSchema.type] {
  val R = JsonSchema

  type PersonTuple = (Seq[Char], Option[Role])

  val current = Current
    .schema(
      caseClass(
        "active".narrow -*>: prim(JsonSchema.JsonBool),
        Iso[Boolean, User](User.apply)(u => u.active)
      )
    )
    .schema(
      caseClass(
        "rights".narrow -*>: seq(prim(JsonSchema.JsonString)),
        Iso[List[String], Admin](Admin.apply)(_.rights)
      )
    )
    .schema(
      (u: Schema[User], a: Schema[Admin]) =>
        sealedTrait(
          "user".narrow -+>: u :+:
            "admin".narrow -+>: a,
          Iso[User \/ Admin, Role] {
            case -\/(u) => u
            case \/-(a) => a
          } {
            case u @ User(_)  => -\/(u)
            case a @ Admin(_) => \/-(a)
          }
        )
    )
    .schema(
      (r: Schema[Role]) =>
        caseClass(
          "name".narrow -*>: prim(JsonSchema.JsonString) :*:
            "role".narrow -*>: optional(
            r
          ),
          Iso[(String, Option[Role]), Person]((Person.apply _).tupled)(p => (p.name, p.role))
        )
    )
    .schema(
      (p: Schema[Person]) =>
        iso(
          SchemaZ[Person, Person](p),
          Person.personToTupleIso
        )
    )

  val person            = current.lookup[Person]
  val personTupleSchema = current.lookup[PersonTuple]
}
