import scalaz._, schema._
import monocle._

import shapeless.syntax.singleton._

object module extends JsonModule[JsonSchema.type] with HasMigration[JsonSchema.type] {

//  import Transform._

  val R = JsonSchema

  implicit val primToEncoderNT = new (JsonSchema.Prim ~> Json.EncoderA) {

    def apply[A](fa: JsonSchema.Prim[A]): Json.EncoderA[A] = { a =>
      fa match {
        case JsonSchema.JsonNumber => a.toString
        case JsonSchema.JsonBool   => a.toString
        case JsonSchema.JsonString => s""""$a""""
        case JsonSchema.JsonNull   => "null"
      }
    }
  }

  final case class Person(name: String, role: Option[Role])
  sealed trait Role
  final case class User(active: Boolean, username: String)       extends Role
  final case class Admin(username: String, rights: List[String]) extends Role

  val burns =
    Person(
      "Motgommery Burns",
      Some(Admin(username = "releaseTheHounds", List("nuclear plant owner", "evil masternind")))
    )
  val homer = Person("Homer Simpson", Some(User(active = true, username = "SpiderPig")))

  trait Lookup[Re, A] {
    def apply(registry: Re): BareSchema[A]
  }

  trait LowPrioLookup {
    implicit def rightLookup[R1, RT, A](implicit rest: Lookup[RT, A]): Lookup[(R1, RT), A] =
      new Lookup[(R1, RT), A] {
        def apply(registry: (R1, RT)): BareSchema[A] = rest(registry._2)
      }
  }

  object Lookup extends LowPrioLookup {
    implicit def leftLookup[RR, A]: Lookup[(BareSchema[A], RR), A] =
      new Lookup[(BareSchema[A], RR), A] {
        def apply(registry: (BareSchema[A], RR)): BareSchema[A] = registry._1
      }
  }

  class Registry[Re](registry: Re) { self =>
    def lookup[A](implicit l: Lookup[Re, A]): BareSchema[A] = l(registry)

    def addEntry[A](mkEntry: Registry[Re] => BareSchema[A]): Registry[(BareSchema[A], Re)] =
      new Registry((mkEntry(self), registry))
  }

  type Id[A]       = A
  type FromPers[A] = BareSchema[Person] => A

  trait Domain {

    def mkUser =
      record(
        "active".narrow -*>: prim(JsonSchema.JsonBool) :*: "username".narrow -*>: prim(
          JsonSchema.JsonString
        ),
        Iso[(Boolean, String), User]((User.apply _).tupled)(u => (u.active, u.username))
      )

    val mkAdmin = record(
      "username".narrow -*>: prim(JsonSchema.JsonString) :*: "rights".narrow -*>: seq(
        prim(JsonSchema.JsonString)
      ),
      Iso[(String, List[String]), Admin]((Admin.apply _).tupled)(a => (a.username, a.rights))
    )

    def mkRole[Re: Registry](implicit u: Lookup[Re, User], a: Lookup[Re, Admin]) =
      (r: Registry[Re]) =>
        union(
          "user".narrow -+>: r.lookup[User] :+:
            "admin".narrow -+>: r.lookup[Admin],
          Iso[User \/ Admin, Role] {
            case -\/(u) => u
            case \/-(a) => a
          } {
            case u @ User(_, _)  => -\/(u)
            case a @ Admin(_, _) => \/-(a)
          }
        )

    def mkPerson[Re](reg: Registry[Re])(implicit r: Lookup[Re, Role]) =
      record(
        "name".narrow -*>: prim(JsonSchema.JsonString) :*:
          "role".narrow -*>: optional(
          reg.lookup[Role]
        ),
        Iso[(String, Option[Role]), Person]((Person.apply _).tupled)(p => (p.name, p.role))
      )

  }

  trait Version2 extends Domain {

    type Types = (BareSchema[Admin], (BareSchema[User], Unit))

    implicit val registry: Registry[Types] = new Registry(())
      .addEntry(_ => mkUser)
      .addEntry(_ => mkAdmin)

    val withRole = registry.addEntry(mkRole[Types])

  }

  object V2 extends Version2

  trait Version1 extends Version2 {

    val reg = withRole.addEntry(_ => userV1)

    def userV1 = mkUser.addField("active".narrow, false)
  }

  object V1 extends Version1 {}

}
