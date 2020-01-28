package schemaz

import scalaz.{ -\/, \/, \/- }

import shapeless.syntax.singleton._

final case class Person(name: String, role: Option[Role])
sealed trait Role
final case class User(active: Boolean)       extends Role
final case class Admin(rights: List[String]) extends Role

object Person {

  private def f(p: Person): (Seq[Char], Option[Role]) = (p.name.toSeq, p.role)
  private def g(t: (Seq[Char], Option[Role])): Person = Person(t._1.mkString, t._2)

  val personToTupleIso = NIso[Person, (Seq[Char], Option[Role])](f, g)
}

trait TestModule extends SchemaModule[JsonSchema.type] with Versioning[JsonSchema.type] {
  override val realisation = JsonSchema

  type PersonTuple = (Seq[Char], Option[Role])

  val role = (u: SchemaZ[User], a: SchemaZ[Admin]) =>
    sealedTrait(
      "user".narrow -+> u :+:
        "admin".narrow -+> a,
      NIso[User \/ Admin, Role]({
        case -\/(u) => u
        case \/-(a) => a
      }, {
        case u @ User(_)  => -\/(u)
        case a @ Admin(_) => \/-(a)
      })
    )

  val current = Current
    .schema(
      caseClass(
        "active".narrow -*> prim(JsonSchema.JsonBool),
        NIso[Boolean, User](User.apply, u => u.active)
      )
    )
    .schema(
      caseClass(
        "rights".narrow -*> seq(prim(JsonSchema.JsonString)),
        NIso[List[String], Admin](Admin.apply, _.rights)
      )
    )
    .schema(
      role
    )
    .schema(
      (r: SchemaZ[Role]) =>
        caseClass(
          "name".narrow -*> prim(JsonSchema.JsonString) :*:
            "role".narrow -*> optional(
            r
          ),
          NIso[(String, Option[Role]), Person]((Person.apply _).tupled, p => (p.name, p.role))
        )
    )
  /*.schema(
      (p: SchemaZ[Person]) =>
        iso[Person, PersonTuple](
          p,
          Person.personToTupleIso
        )
    )
   */
  val person = current.lookup[Person]
  // val personTupleSchema = current.lookup[PersonTuple]
}
