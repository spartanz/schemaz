import scalaz._, schema._
import monocle._

//import shapeless._

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
      "Montgommery Burns",
      Some(Admin(username = "releaseTheHounds", List("nuclear plant owner", "evil masternind")))
    )
  val homer = Person("Homer Simpson", Some(User(active = true, username = "SpiderPig")))

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

  trait Replace[Tpe, Re, A] {
    def apply(registry: Re, replacement: Ctr[Tpe, A]): Re
  }

  object Replace {
    implicit def rightReplace[Tpe, A, RT]: Replace[Tpe, (Ctr[Tpe, A], RT), A] =
      new Replace[Tpe, (Ctr[Tpe, A], RT), A] {
        override def apply(
          registry: (Ctr[Tpe, A], RT),
          replacement: Ctr[Tpe, A]
        ): (Ctr[Tpe, A], RT) = (replacement, registry._2)
      }

    implicit def leftReplace[Tpe, A, R1, RT](
      implicit rest: Replace[Tpe, RT, A]
    ): Replace[Tpe, (R1, RT), A] =
      new Replace[Tpe, (R1, RT), A] {
        override def apply(
          registry: (R1, RT),
          replacement: Ctr[Tpe, A]
        ): (R1, RT) =
          (registry._1, rest(registry._2, replacement))
      }
  }

  type Ctr[Re, A] = Registry[Re] => Schema[A]

  case class CtrRegistry[Types, Re](registry: Re) {

    def addEntry[A](ctr: Ctr[Types, A]): CtrRegistry[(Schema[A], Types), (Ctr[Types, A], Re)] =
      new CtrRegistry[(Schema[A], Types), (Ctr[Types, A], Re)]((ctr, registry))

    def replace[Tpe, A](
      replacement: Ctr[Tpe, A]
    )(implicit replace: Replace[Tpe, Re, A]): CtrRegistry[Types, Re] =
      new CtrRegistry(replace(registry, replacement))
  }

  trait Build[Tpe, Reg] {
    def apply(ctrReg: CtrRegistry[Tpe, Reg]): Registry[Tpe]
  }

  object Build {
    implicit val buildEmpty: Build[Unit, Unit] = new Build[Unit, Unit] {
      def apply(ctrReg: CtrRegistry[Unit, Unit]): Registry[Unit] = new Registry(ctrReg.registry)
    }

    implicit def buildRegistry[A, Tpe, Reg](
      implicit rest: Build[Tpe, Reg]
    ): Build[(Schema[A], Tpe), (Ctr[Tpe, A], Reg)] =
      new Build[(Schema[A], Tpe), (Ctr[Tpe, A], Reg)] {

        def apply(
          ctrReg: CtrRegistry[(Schema[A], Tpe), (Ctr[Tpe, A], Reg)]
        ): Registry[(Schema[A], Tpe)] = {
          val tail = rest(new CtrRegistry[Tpe, Reg](ctrReg.registry._2))

          new Registry((ctrReg.registry._1(tail), tail.registry))
        }
      }
  }

  object CtrRegistry {
    def empty: CtrRegistry[Unit, Unit] = new CtrRegistry(())

    def build[Tpe, Reg](ctrReg: CtrRegistry[Tpe, Reg])(implicit b: Build[Tpe, Reg]): Registry[Tpe] =
      b(ctrReg)
  }

  case class Registry[Re](registry: Re) { self =>

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

  /*
  trait Changelog[X, RA, A] {}
  final case class Init[X, RA, A](create: X => SchemaZ[RA, A]) extends Changelog[X, RA, A]
  final case class Migrate[X, RA, RX, A](
    base: Changelog[X, RA, A],
    mig: SchemaZ[RA, A] => SchemaZ[RX, A]
  ) extends Changelog[X, RX, A]

  object Migrate {

    def create[X, RA, RX, A](base: Changelog[X, RA, A])(mig: SchemaZ[RA, A] => SchemaZ[RX, A]) =
      Migrate(base, mig)
  }

  final case class Prodm[X0, X1, R0, R1, A0, A1, RX, X](
    l: Changelog[X0, R0, A0],
    r: Changelog[X1, R1, A1],
    combine: Changelog[(SchemaZ[R0, A0], SchemaZ[R1, A1]), RX, X]
  ) extends Changelog[(X0, X1), RX, X]
   */
  def mkUser =
    (_: Registry[Unit]) =>
      record(
        "active".narrow -*>: prim(JsonSchema.JsonBool) :*: "username".narrow -*>: prim(
          JsonSchema.JsonString
        ),
        Iso[(Boolean, String), User]((User.apply _).tupled)(u => (u.active, u.username))
      )

//  val initU = Init(mkUser)
//  val v1    = Migrate.create(initU)(_.addField("active".narrow, false))

  val mkAdmin =
    record(
      "username".narrow -*>: prim(JsonSchema.JsonString) :*: "rights".narrow -*>: seq(
        prim(JsonSchema.JsonString)
      ),
      Iso[(String, List[String]), Admin]((Admin.apply _).tupled)(a => (a.username, a.rights))
    )

  def mkRole[Re: Provides[User]#T: Provides[Admin]#T] =
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

  type WithRegistry[A, RA, Re] = Registry[Re] => SchemaZ[RA, A]

  def mkPerson[Re: Provides[Role]#T] =
    (reg: Registry[Re]) =>
      record(
        "name".narrow -*>: prim(JsonSchema.JsonString) :*:
          "role".narrow -*>: optional(
          reg.lookup[Role]
        ),
        Iso[(String, Option[Role]), Person]((Person.apply _).tupled)(p => (p.name, p.role))
      )

  val version2 = CtrRegistry.empty
    .addEntry(mkUser)
    .addEntry(_ => mkAdmin)
    .addEntry(mkRole)
    .addEntry(mkPerson)

  val version2Reg = CtrRegistry.build(version2)

  def userV1 = mkUser.andThen(_.addField("active".narrow, false))

  val version1 = version2
    .replace(userV1)
//    .addEntry(mkRole)

  // val version1Reg = CtrRegistry.build(version1)
}
