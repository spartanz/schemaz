import scalaz._, schema._, Json._ //, Representation._
import monocle._

import shapeless._
import shapeless.syntax.singleton._

object module extends JsonModule[JsonSchema.type] with HasMigration[JsonSchema.type] {

  import Transform._

  val R = JsonSchema
  import R._
  final case class Person(name: String, role: Option[Role])
  sealed trait Role
  final case class User(active: Boolean, boss: Person) extends Role
  final case class Admin(rights: List[String])         extends Role

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

  implicit val primToEncoderNT = new (R.Prim ~> EncoderA) {

    def apply[A](fa: R.Prim[A]): EncoderA[A] = { a =>
      fa match {
        case JsonNumber => a.toString
        case JsonBool   => a.toString
        case JsonString => s""""$a""""
        case JsonNull   => "null"
      }
    }
  }

  val p = person(person(null))

  val path = "role".narrow :: "user".narrow :: "active".narrow :: HNil

  //val path = "role".narrow :: HNil

  type Name = Witness.`"name"`.T
  type Foo  = Witness.`"foo"`.T
  type Bar  = Witness.`"bar"`.T

//  import AtPath._

//  val lookup = AtPath(p, path)

  val sp = "bar".narrow -*>: prim(JsonSchema.JsonString) :*: "foo".narrow -*>: prim(
    JsonSchema.JsonBool
  )
  val s  = record(sp, Iso.id[(String, Boolean)])
  val s2 = prim(JsonSchema.JsonNumber)

  val u = union("s".narrow -+>: s :+: "s2".narrow -+>: s2, Iso.id[(String, Boolean) \/ BigDecimal])

  val trans =
    ((_: Schema[R.Prim[Boolean], Boolean]) => iso(unit, Iso[Unit, Boolean](_ => true)(_ => ())))

  val trans2 = (
    (_: Schema[R.Prim[BigDecimal], BigDecimal]) =>
      iso(unit, Iso[Unit, BigDecimal](_ => BigDecimal(0))(_ => ()))
    )

  val t = Transform(
    u,
    "s".narrow :: "foo".narrow :: HNil,
    trans
  )

  val t2 = Transform(
    t,
    "s2".narrow :: HNil,
    trans2
  )

  //import Representation._

  /*(schema:Schema[
      RRecord[scalaz.schema.Representation.RProd[Bar -*> R.Prim[String],String, Foo -*> R.Prim[Boolean],Boolean],(String, Boolean),(String, Boolean)],
      (String,Boolean)
    ])*/

  val t3 = Partial(
    u,
    "s".narrow :: HNil
  ).transform(
    schema => {
      val d = DerivationTo[Schema]
      d.rec(schema)(
          tplSchema => {
            d.prod(tplSchema)(
              bar => d.const(bar)(bar),
              foo => d.const(foo)(unit)
            )(
              (bar, _) => bar
            )
          }
        )(
          (_, baseSchema) =>
            record(baseSchema, Iso[String, (String, Boolean)](str => (str, true))(tpl => tpl._1))
        )
        .to
    }
  )

  val added = new AddField("s".narrow :: "foo".narrow :: HNil, prim(JsonSchema.JsonBool), true)(u)

  val burns = Person("Montgommery Burns", Some(Admin(List("billionaire", "evil mastermind"))))
  val homer = Person("Homer Simpson", Some(User(true, burns)))

}
