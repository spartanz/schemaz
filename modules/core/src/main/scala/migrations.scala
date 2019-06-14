package scalaz

package schema

trait Migrations[R <: Realisation] extends SchemaModule[R] {

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

  trait Replace[Re, A] {
    type Tpe
    def apply(registry: Re, replacement: Ctr[Tpe, A]): Re
  }

  object Replace {

    type Aux[Re, A, Tpe0] = Replace[Re, A] { type Tpe = Tpe0 }

    implicit def rightReplace[Tpe0, A, RT]: Replace.Aux[(Ctr[Tpe0, A], RT), A, Tpe0] =
      new Replace[(Ctr[Tpe0, A], RT), A] {
        type Tpe = Tpe0
        override def apply(
          registry: (Ctr[Tpe, A], RT),
          replacement: Ctr[Tpe, A]
        ): (Ctr[Tpe, A], RT) = (replacement, registry._2)
      }

    implicit def leftReplace[Tpe0, A, R1, RT](
      implicit rest: Replace.Aux[RT, A, Tpe0]
    ): Replace.Aux[(R1, RT), A, Tpe0] =
      new Replace[(R1, RT), A] {
        type Tpe = Tpe0
        override def apply(
          registry: (R1, RT),
          replacement: Ctr[Tpe0, A]
        ): (R1, RT) =
          (registry._1, rest(registry._2, replacement))
      }
  }

  type Ctr[Re, A] = Registry[Re] => Schema[A]

  case class CtrRegistry[Types, Re](registry: Re) {

    def addEntry[A](ctr: Ctr[Types, A]): CtrRegistry[(Schema[A], Types), (Ctr[Types, A], Re)] =
      new CtrRegistry[(Schema[A], Types), (Ctr[Types, A], Re)]((ctr, registry))

    def migrate[A](
      implicit replace: Replace[Re, A]
    ): Ctr[replace.Tpe, A] => CtrRegistry[Types, Re] =
      (replacement: Ctr[replace.Tpe, A]) => new CtrRegistry(replace(registry, replacement))
  }

  object CtrRegistry {
    def empty: CtrRegistry[Unit, Unit] = new CtrRegistry(())

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

  case class Registry[Re](registry: Re) { self =>

    def lookup[A](implicit l: Lookup[Re, A]): Schema[A] = l(registry)

    def addEntry[A](mkEntry: Registry[Re] => Schema[A]): Registry[(Schema[A], Re)] =
      new Registry((mkEntry(self), registry))

  }

  object Registry {
    val empty: Registry[Unit] = new Registry(())

    def build[Tpe, Reg](ctrReg: CtrRegistry[Tpe, Reg])(implicit b: Build[Tpe, Reg]): Registry[Tpe] =
      b(ctrReg)

  }

}
