package scalaz

package schema

import monocle.Iso

/*
final case class Person(name: String, role: Option[Role])
sealed trait Role
final case class User(active: Boolean, boss: Person) extends Role
final case class Admin(rights: List[String])         extends Role
 */

final case class Foo(s: String, b: Boolean, i: BigDecimal)

trait TestModule extends JsonModule[JsonSchema.type] {
  val R = JsonSchema

  /*type PersonTuple = (Seq[Char], Option[Role])

  val user = record(
    "active" -*>: prim(JsonSchema.JsonBool) :*: "boss" -*>: self[Person](person),
    Iso[(Boolean, Person), User]((User.apply _).tupled)(u => (u.active, u.boss))
  )*/

  /*  val admin = record(
    "rights" -*>: seq(prim(JsonSchema.JsonString)),
    Iso[List[String], Admin](Admin.apply)(_.rights)
  )*/

  val foo /*: Schema[
    RecordR[
      (
        scalaz.schema.FieldR[R.Prim[String]],
        (
          scalaz.schema.FieldR[R.Prim[Boolean]],
          scalaz.schema.FieldR[R.Prim[BigDecimal]]
        )
      ),
      scalaz.schema.Foo
    ],
    scalaz.schema.Foo
  ]*/ = record(
    "s" -*>: prim(JsonSchema.JsonString) :*:
      "b" -*>: prim(JsonSchema.JsonBool) :*:
      "i" -*>: prim(JsonSchema.JsonNumber),
    Iso[(String, (Boolean, BigDecimal)), Foo](
      tpl => Foo(tpl._1, tpl._2._1, tpl._2._2)
    )(
      x => (x.s, (x.b, x.i))
    )
  )

  val derivation = DerivationTo[Schema]
    .rec(foo)(
      (d, pSchema) =>
        d.prod(pSchema)(
          (d, l) => d.const(l)(unit),
          (d, r) => d.const(r)(r)
        )(
          (_, rd) => rd
        )
    )(
      (_, schema) =>
        recordUnsafe(
          schema,
          Iso[(Boolean, BigDecimal), Foo](
            tpl => Foo("default", tpl._1, tpl._2)
          )(
            foo => (foo.b, foo.i)
          )
        )
    )

  val newFoo = derivation.to

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

  val newEnc = newFoo.to[Json.Encoder]

  /*val role = union(
    "user" -+>: user :+:
      "admin" -+>: admin,
    Iso[User \/ Admin, Role] {
      case -\/(u) => u
      case \/-(a) => a
    } {
      case u @ User(_, _) => -\/(u)
      case a @ Admin(_)   => \/-(a)
    }
  )

  def person = record(
    "name" -*>: prim(JsonSchema.JsonString) :*:
      "role" -*>: optional(
      role
    ),
    Iso[(String, Option[Role]), Person]((Person.apply _).tupled)(p => (p.name, p.role))
  )*/

}
