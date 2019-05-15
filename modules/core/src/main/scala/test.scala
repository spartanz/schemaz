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

  //final case class Thing(s: String)
  final case class Thing(s: String, b: Boolean)

  object Thing {
    def v1(s: String): Thing       = Thing(s, true)
    def toV1(thing: Thing): String = thing.s
  }

  val sp = "bar".narrow -*>: prim(JsonSchema.JsonString) :*: "foo".narrow -*>: prim(
    JsonSchema.JsonBool
  )

  val sp0 = "bar".narrow -*>: prim(JsonSchema.JsonString)

  val s = record(
    sp,
    Iso[(String, Boolean), Thing](tpl => Thing(tpl._1, tpl._2))(thing => (thing.s, thing.b))
  )

  val s0 = record(
    iso(sp0, Iso[String, (String, Boolean)](s => (s, true))(tpl => tpl._1)),
    Iso[(String, Boolean), Thing](tpl => Thing(tpl._1, tpl._2))(thing => (thing.s, thing.b))
  )

  val s2 = prim(JsonSchema.JsonNumber)

  def uF[RT](thingSchema: Schema[RT, Thing]) =
    union("s".narrow -+>: thingSchema :+: "s2".narrow -+>: s2, Iso.id[Thing \/ BigDecimal])

  val u  = uF(s)
  val u0 = uF(s0)

  /* val trans = ((_: Schema[R.Prim[Boolean], Boolean]) => iso(unit, Iso[Unit, Boolean](_ => true)(_ => ())))

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
  )*/

  /*
  transforms the `s schema located at path "s" as follows

  scalaz.schema.Representation.RRecord[
    scalaz.schema.Representation.RProd[
      String("bar") -*> module.R.Prim[String], String,
      String("foo") -*> module.R.Prim[Boolean], Boolean
    ],
    (String, Boolean),
    (String, Boolean)
  ]


  scalaz.schema.Representation.RRecord[
    String("bar") -*> module.R.JsonPrim[String],
    String,
    (String, Boolean)
  ]
   */
  //will ommit foo field when serializing. We need to touch the iso on the record but it is closer to what the schema was in the previous version
  val t3 = Partial(
    u,
    "s".narrow :: HNil
  ).transform(
      schema => {
        migrate
          .rec(schema)(
            tplSchema => {
              migrate.prod(tplSchema)(
                bar => migrate.const(bar)(bar),
                foo => migrate.const(foo)(unit)
              )(
                (bar, _) => bar
              )
            }
          )(
            (_, baseSchema) => record(baseSchema, Iso[String, Thing](Thing.v1)(Thing.toV1))
          )
          .to
      }
    )
    .to[Encoder]

  //will still print foo but as something of unit schema. we don't need to touch the record iso because we use an IsoSchema to suspend a constant value over the product term of this field
  val t4 = Partial(
    u,
    "s".narrow :: HNil
  ).transform(
      schema => {
        migrate
          .rec(schema)(
            tplSchema => {
              migrate.prod(tplSchema)(
                bar => migrate.const(bar)(bar),
                foo =>
                  migrate
                    .const(foo)("foo".narrow -*>: iso(unit, Iso[Unit, Boolean](_ => true)(_ => ())))
              )(
                (bar, foo) => bar :*: foo
              )
            }
          )(
            (iso, baseSchema) => record(baseSchema, iso)
          )
          .to
      }
    )
    .to[Encoder]

  //basically the same as t3
  val t5 = Partial(
    u,
    "s".narrow :: HNil
  ).transform(
      schema => {
        migrate
          .rec(schema)(
            tplSchema => {
              migrate.prod(tplSchema)(
                bar => migrate.const(bar)(bar),
                foo => migrate.const(foo)(unit)
              )(
                (bar, _) => bar
              )
            }
          )(
            (recIso, baseSchema) =>
              /*record(
                iso(baseSchema, Iso[String, (String, Boolean)](s => (s, true))(tpl => tpl._1)),
                recIso
              )*/
              record(
                baseSchema,
                (Iso[String, (String, Boolean)](s => (s, true))(tpl => tpl._1)).composeIso(recIso)
              )
          )
          .to
      }
    )
    .to[Encoder]

  val added    = new AddField("s".narrow :: "foo".narrow :: HNil, prim(JsonSchema.JsonBool), true)(u)
  val addedEnc = added.to[Encoder]

  val burns = Person("Montgommery Burns", Some(Admin(List("billionaire", "evil mastermind"))))
  val homer = Person("Homer Simpson", Some(User(true, burns)))

}
