package scalaz.schema

import monocle.{Getter, Iso}
import monocle.macros.GenPrism
import scalaz.Scalaz.^

final case class Person(name: String, role: Option[Role])
sealed trait Role
final case class User(active: Boolean)       extends Role
final case class Admin(rights: List[String]) extends Role

object Person {
  val name = Getter[Person, String](_.name)

  def setRole(r1: Role): Person => Person =
    (p: Person) => p.copy(role = Some(r1))
  val role = monocle.Optional[Person, Role](_.role)(setRole(_))

  val user   = GenPrism[Role, User]
  val admin  = GenPrism[Role, Admin]
  val active = Getter[User, Boolean](_.active)
  val rights = Getter[Admin, List[String]](_.rights)

  private def f(p: Person): (Seq[Char], Option[Role]) = (p.name.toSeq, p.role)
  private def g(t: (Seq[Char], Option[Role])): Person = Person(t._1.mkString, t._2)

  val personToTupleIso = Iso[Person, (Seq[Char], Option[Role])](f)(g)


  def schema(module:SchemaModule):module.Schema[Person] = {
    import module._
    record[Person](
      ^(
        essentialField[Person, String](
          "name",
          prim(JsonSchema.JsonString),
          Person.name,
          None
        ),
        nonEssentialField[Person, Role](
          "role",
          union[Role](
            branch(
              "user",
              record[User](
                essentialField(
                  "active",
                  prim(JsonSchema.JsonBool),
                  Person.active,
                  None
                ).map(User.apply)
              ),
              Person.user
            ),
            branch(
              "admin",
              record[Admin](
                essentialField(
                  "rights",
                  seq(prim(JsonSchema.JsonString)),
                  Person.rights,
                  None
                ).map(Admin.apply)
              ),
              Person.admin
            )
          ),
          Person.role
        )
      )(Person.apply)
    )
  }
}
