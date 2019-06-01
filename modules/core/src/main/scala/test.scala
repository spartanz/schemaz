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

  trait ForAll[F[_], TC[_]] {
    def apply[A: TC]: F[A]
  }

  trait Lookup[Re, A] {
    def apply(registry: Re): Schema[A]
  }

  trait LowPrioLookup {
    implicit def rightLookup[R1, RT, A](implicit rest: Lookup[RT, A]): Lookup[(R1, RT), A] =
      new Lookup[(R1, RT), A] {
        def apply(registry: (R1, RT)): Schema[A] = rest(registry._2)
      }
  }

  object Lookup extends LowPrioLookup {
    implicit def leftLookup[RR, A]: Lookup[(Schema[A], RR), A] =
      new Lookup[(Schema[A], RR), A] {
        def apply(registry: (Schema[A], RR)): Schema[A] = registry._1
      }
  }

  class Registry[Re](registry: Re) { self =>
    def lookup[A](implicit l: Lookup[Re, A]): Schema[A] = l(registry)

    def addEntry[A](mkEntry: Registry[Re] => Schema[A]): Registry[(Schema[A], Re)] =
      new Registry((mkEntry(self), registry))
  }

  object Registry {
    val empty: Registry[Unit] = new Registry(())

  }

  trait Provides[A] {
    type T[Re] = Lookup[Re, A]
  }

  trait EList[T]

  final case class ENil[T]() extends EList[T] {
    def :/: [H[_]](head: H[T]): ECons[H, ENil, T] = ECons(head, this)
  }
  final case class ECons[H[_], T[x] <: EList[x], A](head: H[A], tail: T[A]) extends EList[A] {
    def :/: [G[_]](elem: G[A]): ECons[G, ECons[H, T, ?], A] = ECons(elem, this)
  }

  object EList {
    implicit def emptyEList[A]: ENil[A] = new ENil[A]()

    implicit def consEList[A, H[A], T[A] <: EList[A]](
      implicit head: H[A],
      tail: T[A]
    ): ECons[H, T, A] = ECons(head, tail)

    implicit def selectHead[TC[_], T[x] <: EList[x], A](implicit list: ECons[TC, T, A]): TC[A] =
      list.head

//    implicit def selectTail[TC[_], ]
  }

  val mkUser =
    (_: Unit) =>
      record(
        "active".narrow -*>: prim(JsonSchema.JsonBool) :*: "username".narrow -*>: prim(
          JsonSchema.JsonString
        ),
        Iso[(Boolean, String), User]((User.apply _).tupled)(u => (u.active, u.username))
      )

  val uv1 = mkUser.andThen(_.addField("active".narrow, false))

  val mkAdmin = record(
    "username".narrow -*>: prim(JsonSchema.JsonString) :*: "rights".narrow -*>: seq(
      prim(JsonSchema.JsonString)
    ),
    Iso[(String, List[String]), Admin]((Admin.apply _).tupled)(a => (a.username, a.rights))
  )
  /*
  def mkRole[Re: Provides[User]#T: Provides[Admin]#T] =
    new ForAll[WithRegistry[Role, ?], ECons[Provides[User]#T, ECons[Provides[Admin]#T, ENil, ?], ?]] {

      def apply[Re: ECons[Provides[User]#T, ECons[Provides[Admin]#T, ENil, ?], ?]] =
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
    }

   */
  type WithRegistry[A, Re] = Registry[Re] => Schema[A]

  def mkPerson = new ForAll[WithRegistry[Person, ?], Provides[Role]#T] {

    def apply[Re: Provides[Role]#T]: WithRegistry[Person, Re] =
      (reg: Registry[Re]) =>
        record(
          "name".narrow -*>: prim(JsonSchema.JsonString) :*:
            "role".narrow -*>: optional(
            reg.lookup[Role]
          ),
          Iso[(String, Option[Role]), Person]((Person.apply _).tupled)(p => (p.name, p.role))
        )
  }
  /*
  val version2 = Registry.empty
    .addEntry(_ => mkUser)
    .addEntry(_ => mkAdmin)
    .addEntry(mkRole)
    .addEntry(mkPerson)

  def userV1 = mkUser.addField("active".narrow, false)

  val version1 = version2
    .addEntry(_ => userV1)
//    .addEntry(mkRole)
 */
}
