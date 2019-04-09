package scalaz

package schema

import monocle.Iso

final case class Person(name: String, role: Option[Role])
sealed trait Role
final case class User(active: Boolean, boss: Person) extends Role
final case class Admin(rights: List[String])         extends Role

final case class Foo(s: String, b: Boolean, i: BigDecimal)

trait TestModule extends JsonModule[JsonSchema.type] {
  val R = JsonSchema

  val boss       = Person("Alfred", None)
  val testPerson = Person("Alfred the Second", Some(User(true, boss)))

  type PersonTuple = (Seq[Char], Option[Role])

  def user[Repr](pers: Schema[Repr, Person]) = record(
    "active" -*>: prim(JsonSchema.JsonBool) :*: "boss" -*>: self[Person](pers),
    Iso[(Boolean, Person), User]((User.apply _).tupled)(u => (u.active, u.boss))
  )

  val admin = record(
    "rights" -*>: seq(prim(JsonSchema.JsonString)),
    Iso[List[String], Admin](Admin.apply)(_.rights)
  )

  def role[Repr](pers: Schema[Repr, Person]) = union(
    "user" -+>: user(pers) :+:
      "admin" -+>: admin,
    Iso[User \/ Admin, Role] {
      case -\/(u) => u
      case \/-(a) => a
    } {
      case u @ User(_, _) => -\/(u)
      case a @ Admin(_)   => \/-(a)
    }
  )

  def person(pers: Schema_[Person]) = record(
    "name" -*>: prim(JsonSchema.JsonString) :*:
      "role" -*>: optional(
      role[pers.Repr](pers)
    ),
    Iso[(String, Option[Role]), Person]((Person.apply _).tupled)(p => (p.name, p.role))
  )

  val personSchema = person(person(null))

  implicit val primToEncoderNT = new (JsonSchema.JsonPrim ~> Json.Encoder) {

    def apply[A](fa: JsonSchema.JsonPrim[A]): Json.Encoder[A] = { a =>
      fa match {
        case JsonSchema.JsonNumber => a.toString
        case JsonSchema.JsonBool   => a.toString
        case JsonSchema.JsonString => s""""$a""""
        case JsonSchema.JsonNull   => "null"
      }
    }
  }

  def personDerivation[TRepr](
    ref: => Derivation[Schema, personSchema.Repr, Person, TRepr, Person]
  ) =
    DerivationTo[Schema].rec(personSchema)(
      (d, personFields) =>
        d.prod(personFields)(
          (d, l) => d.const(l)(unit),
          (d, r) =>
            d.field(r)(
              (d, b) =>
                d.iso(b)(
                  (d, b) =>
                    d.sum(b)(
                      (d, l) =>
                        d.union(l)(
                          (d, b) =>
                            d.sum(b)(
                              (d, l) =>
                                d.branch(l)(
                                  (d, b) =>
                                    d.rec(b)(
                                      (d, userFields) =>
                                        d.prod(userFields)(
                                          (d, l) => d.const(l)(l),
                                          (d, r) =>
                                            d.field(r)(
                                              (d, b) => d.const(b)(self(ref.to))
                                            )(
                                              (id, x) => (id -*>: x).toSchema
                                            )
                                        )(
                                          (l, r) => l :*: r
                                        )
                                    )(
                                      (iso, x) => recordUnsafe(x, iso)
                                    )
                                )(
                                  (id, x) => (id -+>: x).toSchema
                                ),
                              (d, r) => d.const(r)(r)
                            )(
                              (l, r) => l :+: r
                            )
                        )(
                          (iso, x) => unionUnsafe(x, iso)
                        ),
                      (d, r) => d.const(r)(r)
                    )(
                      (l, r) => l :+: r
                    )
                )(
                  (isoI, x) => iso(x, isoI)
                )
            )(
              (id, x) => (id -*>: x).toSchema
            )
        )(
          (_, r) => r
        )
    )(
      (_, x) =>
        recordUnsafe(
          x,
          Iso[Option[Role], Person](Person("default", _))(_.role)
        )
    )

  val newPerson = personDerivation(personDerivation(null)).to
  val newEnc    = newPerson.to[Json.Encoder]

}
