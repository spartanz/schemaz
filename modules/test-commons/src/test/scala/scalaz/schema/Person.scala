package scalaz.schema

import monocle.Getter
import monocle.macros.GenPrism

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
}
