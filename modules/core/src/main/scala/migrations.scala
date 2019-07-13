package schemaz

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

  trait CtrLookup[Re, A] {
    type Repr
    type Deps
    def apply(registry: Re): CtrRegistry.Entry.Aux[A, Repr, Deps]
  }

  trait LowPrioCtrLookup {
    implicit def rightLookup[R1, RT, A, R0, D0](
      implicit rest: CtrLookup.Aux[RT, A, R0, D0]
    ): CtrLookup.Aux[(R1, RT), A, R0, D0] =
      new CtrLookup[(R1, RT), A] {
        type Repr = R0
        type Deps = D0
        def apply(registry: (R1, RT)): CtrRegistry.Entry.Aux[A, R0, D0] = rest(registry._2)
      }
  }

  object CtrLookup extends LowPrioCtrLookup {
    type Aux[Re, A, R0, D0] = CtrLookup[Re, A] { type Repr = R0; type Deps = D0 }
    implicit def leftLookup[RR, A, R0, D]
      : CtrLookup.Aux[(CtrRegistry.Entry.Aux[A, R0, D], RR), A, R0, D] =
      new CtrLookup[(CtrRegistry.Entry.Aux[A, R0, D], RR), A] {
        type Repr = R0
        type Deps = D

        def apply(
          registry: (CtrRegistry.Entry.Aux[A, R0, D], RR)
        ): CtrRegistry.Entry.Aux[A, R0, D] =
          registry._1
      }
  }

  trait Replace[Re, A, R1, D] {
    type Out
    def apply(registry: Re, replacement: CtrRegistry.Entry.Aux[A, R1, D]): Out
  }

  object Replace {

    type Aux[Re, A, R1, D, Re1] = Replace[Re, A, R1, D] { type Out = Re1 }

    implicit def rightReplace[A, RA, RA1, D, RT]: Replace.Aux[
      (CtrRegistry.Entry.Aux[A, RA, D], RT),
      A,
      RA1,
      D,
      (CtrRegistry.Entry.Aux[A, RA1, D], RT)
    ] =
      new Replace[(CtrRegistry.Entry.Aux[A, RA, D], RT), A, RA1, D] {
        type Out = (CtrRegistry.Entry.Aux[A, RA1, D], RT)
        override def apply(
          registry: (CtrRegistry.Entry.Aux[A, RA, D], RT),
          replacement: CtrRegistry.Entry.Aux[A, RA1, D]
        ): (CtrRegistry.Entry.Aux[A, RA1, D], RT) = (replacement, registry._2)
      }

    implicit def leftReplace[H, A, R1, D, RT, RT1](
      implicit rest: Replace.Aux[RT, A, R1, D, RT1]
    ): Replace.Aux[(H, RT), A, R1, D, (H, RT1)] =
      new Replace[(H, RT), A, R1, D] {
        type Out = (H, RT1)
        override def apply(
          registry: (H, RT),
          replacement: CtrRegistry.Entry.Aux[A, R1, D]
        ): (H, RT1) =
          (registry._1, rest(registry._2, replacement))
      }
  }

  case class CtrRegistry[Types, Re](registry: Re) {

    def addEntry[A, RA, D](ctr: D => SchemaZ[RA, A])(
      implicit add: AddEntry[Types, D, A, RA]
    ): CtrRegistry[(Schema[A], Types), (CtrRegistry.Entry.Aux[A, RA, Types], Re)] =
      new CtrRegistry[(Schema[A], Types), (CtrRegistry.Entry.Aux[A, RA, Types], Re)](
        (add(CtrRegistry.Entry(ctr)), registry)
      )

    def migrate[A](
      implicit lookup: CtrLookup[Re, A]
    ): CtrRegistry.Entry.Aux[A, lookup.Repr, lookup.Deps] = lookup(registry)

    def replace[A, R1, D, Re1](newEntry: CtrRegistry.Entry.Aux[A, R1, D])(
      implicit replace: Replace.Aux[Re, A, R1, D, Re1]
    ): CtrRegistry[Types, Re1] = new CtrRegistry[Types, Re1](replace(registry, newEntry))
  }

  trait AddEntry[Re, D, A, RA] {
    def prepare: Re => D

    def apply(
      newEntry: CtrRegistry.Entry.Aux[A, RA, D]
    ): CtrRegistry.Entry.Aux[A, RA, Re] =
      newEntry.pre(prepare)
  }

  object AddEntry {

    implicit def noDependencies[Re, A, RA]: AddEntry[Re, Unit, A, RA] =
      new AddEntry[Re, Unit, A, RA] { def prepare: Re => Unit = _ => () }

    implicit def singleDependency[Re, D, A, RA](
      implicit D: Lookup[Re, D]
    ): AddEntry[Re, Schema[D], A, RA] = new AddEntry[Re, Schema[D], A, RA] {
      def prepare: Re => Schema[D] = (re => D(re))
    }

    implicit def twoDependencies[Re, D1, D2, A, RA](
      implicit D1: Lookup[Re, D1],
      D2: Lookup[Re, D2]
    ): AddEntry[Re, (Schema[D1], Schema[D2]), A, RA] =
      new AddEntry[Re, (Schema[D1], Schema[D2]), A, RA] {
        def prepare: Re => (Schema[D1], Schema[D2]) = (re => (D1(re), D2(re)))
      }
  }

  object CtrRegistry {
    def empty: CtrRegistry[Unit, Unit] = new CtrRegistry(())

    trait Entry[A] {
      type Deps
      type Repr
      val entry: Deps => SchemaZ[Repr, A]
      def pre[D0](f: D0 => Deps): Entry.Aux[A, Repr, D0]
      def post[R0](f: SchemaZ[Repr, A] => SchemaZ[R0, A]): Entry.Aux[A, R0, Deps]
    }

    object Entry {
      type Aux[A, RA, D] = Entry[A] { type Deps = D; type Repr = RA }
      def apply[A, RA, D](ctr: D => SchemaZ[RA, A]): Aux[A, RA, D] = CtrRegistryEntry[A, RA, D](ctr)
    }
    case class CtrRegistryEntry[A, RA, D](entry: D => SchemaZ[RA, A]) extends Entry[A] {
      type Deps = D
      type Repr = RA

      def pre[D0](f: D0 => Deps): Entry.Aux[A, Repr, D0] = Entry(entry.compose(f))

      def post[R0](f: SchemaZ[Repr, A] => SchemaZ[R0, A]): Entry.Aux[A, R0, Deps] =
        Entry(f.compose(entry))

    }
  }

  trait Build[Tpe, Reg] {
    def apply(ctrReg: CtrRegistry[Tpe, Reg]): Registry[Tpe]
  }

  object Build {
    implicit val buildEmpty: Build[Unit, Unit] = new Build[Unit, Unit] {
      def apply(ctrReg: CtrRegistry[Unit, Unit]): Registry[Unit] = new Registry(ctrReg.registry)
    }

    implicit def buildRegistry[A, Tpe, Reg, RA](
      implicit rest: Build[Tpe, Reg]
    ): Build[(Schema[A], Tpe), (CtrRegistry.Entry.Aux[A, RA, Tpe], Reg)] =
      new Build[(Schema[A], Tpe), (CtrRegistry.Entry.Aux[A, RA, Tpe], Reg)] {

        def apply(
          ctrReg: CtrRegistry[(Schema[A], Tpe), (CtrRegistry.Entry.Aux[A, RA, Tpe], Reg)]
        ): Registry[(Schema[A], Tpe)] = {
          val tail = rest(new CtrRegistry[Tpe, Reg](ctrReg.registry._2))

          new Registry((SchemaZ.untag(ctrReg.registry._1.entry(tail.registry)), tail.registry))
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
