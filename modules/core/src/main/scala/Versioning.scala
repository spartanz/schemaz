package schemaz

trait Versioning[R <: Realisation] extends SchemaModule[R] {
  /*
  final val Current: Version[Unit, Unit] = new Version(())

  sealed case class Version[Types, Re](registry: Re)(
    implicit build: Version.Build[Types, Re]
  ) {

    lazy val types: Types = build(registry)

    def schema[P[_, _], A, RA, T](leaf: SchemaZ[P, RA, A, T])(
      implicit add: Version.AddEntry[Types, Unit, A, RA]
    ): Version[(Schema[A], Types), (Version.Entry.Aux[A, RA, Types], Re)] =
      new Version((add(Version.Entry((_: Unit) => leaf)), registry))

    def schema[P[_, _], A, RA, D, T](ctr: D => SchemaZ[P, RA, A, T])(
      implicit add: Version.AddEntry[Types, D, A, RA]
    ): Version[(Schema[A], Types), (Version.Entry.Aux[A, RA, Types], Re)] =
      new Version(
        (add(Version.Entry(ctr)), registry)
      )

    def schema[P[_, _], A, RA, D1, D2, T](ctr: (D1, D2) => SchemaZ[P, RA, A, T])(
      implicit add: Version.AddEntry[Types, (D1, D2), A, RA]
    ): Version[(Schema[A], Types), (Version.Entry.Aux[A, RA, Types], Re)] =
      new Version(
        (add(Version.Entry(ctr.tupled)), registry)
      )

    def schema[P[_, _], A, RA, D1, D2, D3, T](ctr: (D1, D2, D3) => SchemaZ[P, RA, A, T])(
      implicit add: Version.AddEntry[Types, (D1, D2, D3), A, RA]
    ): Version[(Schema[A], Types), (Version.Entry.Aux[A, RA, Types], Re)] =
      new Version(
        (add(Version.Entry(ctr.tupled)), registry)
      )

    def schema[P[_, _], A, RA, D1, D2, D3, D4, T](ctr: (D1, D2, D3, D4) => SchemaZ[P, RA, A, T])(
      implicit add: Version.AddEntry[Types, (D1, D2, D3, D4), A, RA]
    ): Version[(Schema[A], Types), (Version.Entry.Aux[A, RA, Types], Re)] =
      new Version(
        (add(Version.Entry(ctr.tupled)), registry)
      )

    def schema[P[_, _], A, RA, D1, D2, D3, D4, D5, T](
      ctr: (D1, D2, D3, D4, D5) => SchemaZ[P, RA, A, T]
    )(
      implicit add: Version.AddEntry[Types, (D1, D2, D3, D4, D5), A, RA]
    ): Version[(Schema[A], Types), (Version.Entry.Aux[A, RA, Types], Re)] =
      new Version(
        (add(Version.Entry(ctr.tupled)), registry)
      )

    def migrate[A](
      implicit lookup: Version.LookupCtr[Re, A]
    ): Migration[Types, Re, A, lookup.Repr, lookup.Deps] = new Migration(registry, lookup)

    def lookup[A](implicit l: Version.Lookup[Types, A]): Schema[A] = l(types)

  }

  final class Migration[Types, Re, A, RA, D](
    registry: Re,
    lookup: Version.LookupCtr.Aux[Re, A, RA, D]
  ) {

    def change[RA1, Re1](migration: SchemaZ[RA, A] => SchemaZ[RA1, A])(
      implicit replace: Version.Replace.Aux[Re, A, RA1, D, Re1],
      build: Version.Build[Types, Re1]
    ): Version[Types, Re1] =
      new Version[Types, Re1](replace(registry, lookup(registry).post(migration)))
  }

  object Version {

    trait Entry[P[_, _], A, T] {
      type Deps
      type Repr
      val entry: Deps => SchemaZ[P, Repr, A, T]
      def pre[D0](f: D0 => Deps): Entry.Aux[P, A, T, Repr, D0]
      def post[R0](f: SchemaZ[P, Repr, A, T] => SchemaZ[P, R0, A, T]): Entry.Aux[P, A, T, R0, Deps]
    }

    object Entry {
      type Aux[P[_, _], A, T, RA, D] = Entry[P, A, T] { type Deps = D; type Repr = RA }

      def apply[P[_, _], A, T, RA, D](ctr: D => SchemaZ[P, RA, A, T]): Aux[P, A, T, RA, D] =
        VersionEntry[P, A, T, RA, D](ctr)
    }
    case class VersionEntry[P[_, _], A, T, RA, D](entry: D => SchemaZ[P, RA, A, T])
        extends Entry[P, A, T] {
      type Deps = D
      type Repr = RA

      def pre[D0](f: D0 => Deps): Entry.Aux[P, A, T, Repr, D0] = Entry(entry.compose(f))

      def post[R0](
        f: SchemaZ[P, Repr, A, T] => SchemaZ[P, R0, A, T]
      ): Entry.Aux[P, A, T, R0, Deps] =
        Entry(entry.andThen(f))

    }

    trait AddEntry[P[_, _], Re, D, A, RA, T] {
      def prepare: Re => D

      def apply(
        newEntry: Version.Entry.Aux[P, A, T, RA, D]
      ): Version.Entry.Aux[P, A, T, RA, Re] =
        newEntry.pre(prepare)
    }

    object AddEntry {

      implicit def noDependencies[P[_, _], Re, A, RA, T]: AddEntry[P, Re, Unit, A, RA, T] =
        new AddEntry[P, Re, Unit, A, RA, T] { def prepare: Re => Unit = _ => () }

      implicit def singleDependency[P[_, _], Re, D, A, RA, T](
        implicit D: Version.Lookup[Re, D]
      ): AddEntry[P, Re, Schema[D], A, RA, T] = new AddEntry[P, Re, Schema[D], A, RA, T] {
        def prepare: Re => Schema[D] = (re => D(re))
      }

      implicit def twoDependencies[Re, D1, D2, A, RA](
        implicit D1: Version.Lookup[Re, D1],
        D2: Version.Lookup[Re, D2]
      ): AddEntry[Re, (Schema[D1], Schema[D2]), A, RA] =
        new AddEntry[Re, (Schema[D1], Schema[D2]), A, RA] {
          def prepare: Re => (Schema[D1], Schema[D2]) = (re => (D1(re), D2(re)))
        }

      implicit def threeDependencies[Re, D1, D2, D3, A, RA](
        implicit D1: Version.Lookup[Re, D1],
        D2: Version.Lookup[Re, D2],
        D3: Version.Lookup[Re, D3]
      ): AddEntry[Re, (Schema[D1], Schema[D2], Schema[D3]), A, RA] =
        new AddEntry[Re, (Schema[D1], Schema[D2], Schema[D3]), A, RA] {
          def prepare: Re => (Schema[D1], Schema[D2], Schema[D3]) = (re => (D1(re), D2(re), D3(re)))
        }

      implicit def fourDependencies[Re, D1, D2, D3, D4, A, RA](
        implicit D1: Version.Lookup[Re, D1],
        D2: Version.Lookup[Re, D2],
        D3: Version.Lookup[Re, D3],
        D4: Version.Lookup[Re, D4]
      ): AddEntry[Re, (Schema[D1], Schema[D2], Schema[D3], Schema[D4]), A, RA] =
        new AddEntry[Re, (Schema[D1], Schema[D2], Schema[D3], Schema[D4]), A, RA] {

          def prepare: Re => (Schema[D1], Schema[D2], Schema[D3], Schema[D4]) =
            (re => (D1(re), D2(re), D3(re), D4(re)))
        }

      implicit def fiveDependencies[Re, D1, D2, D3, D4, D5, A, RA](
        implicit D1: Version.Lookup[Re, D1],
        D2: Version.Lookup[Re, D2],
        D3: Version.Lookup[Re, D3],
        D4: Version.Lookup[Re, D4],
        D5: Version.Lookup[Re, D5]
      ): AddEntry[Re, (Schema[D1], Schema[D2], Schema[D3], Schema[D4], Schema[D5]), A, RA] =
        new AddEntry[Re, (Schema[D1], Schema[D2], Schema[D3], Schema[D4], Schema[D5]), A, RA] {

          def prepare: Re => (Schema[D1], Schema[D2], Schema[D3], Schema[D4], Schema[D5]) =
            (re => (D1(re), D2(re), D3(re), D4(re), D5(re)))
        }
    }

    trait LookupCtr[Re, A] {
      type Repr
      type Deps
      def apply(registry: Re): Version.Entry.Aux[A, Repr, Deps]
    }

    trait LowPrioLookupCtr {
      implicit def tailLookupCtr[R1, RT, A, R0, D0](
        implicit rest: LookupCtr.Aux[RT, A, R0, D0]
      ): LookupCtr.Aux[(R1, RT), A, R0, D0] =
        new LookupCtr[(R1, RT), A] {
          type Repr = R0
          type Deps = D0
          def apply(registry: (R1, RT)): Version.Entry.Aux[A, R0, D0] = rest(registry._2)
        }
    }

    object LookupCtr extends LowPrioLookupCtr {
      type Aux[Re, A, R0, D0] = LookupCtr[Re, A] { type Repr = R0; type Deps = D0 }
      implicit def headLookupCtr[RR, A, R0, D]
        : LookupCtr.Aux[(Version.Entry.Aux[A, R0, D], RR), A, R0, D] =
        new LookupCtr[(Version.Entry.Aux[A, R0, D], RR), A] {
          type Repr = R0
          type Deps = D

          def apply(
            registry: (Version.Entry.Aux[A, R0, D], RR)
          ): Version.Entry.Aux[A, R0, D] =
            registry._1
        }
    }

    trait Replace[Re, A, R1, D] {
      type Out
      def apply(registry: Re, replacement: Version.Entry.Aux[A, R1, D]): Out
    }

    object Replace {

      type Aux[Re, A, R1, D, Re1] = Replace[Re, A, R1, D] { type Out = Re1 }

      implicit def tailReplace[A, RA, RA1, D, RT]: Replace.Aux[
        (Version.Entry.Aux[A, RA, D], RT),
        A,
        RA1,
        D,
        (Version.Entry.Aux[A, RA1, D], RT)
      ] =
        new Replace[(Version.Entry.Aux[A, RA, D], RT), A, RA1, D] {
          type Out = (Version.Entry.Aux[A, RA1, D], RT)
          override def apply(
            registry: (Version.Entry.Aux[A, RA, D], RT),
            replacement: Version.Entry.Aux[A, RA1, D]
          ): (Version.Entry.Aux[A, RA1, D], RT) = (replacement, registry._2)
        }

      implicit def headReplace[H, A, R1, D, RT, RT1](
        implicit rest: Replace.Aux[RT, A, R1, D, RT1]
      ): Replace.Aux[(H, RT), A, R1, D, (H, RT1)] =
        new Replace[(H, RT), A, R1, D] {
          type Out = (H, RT1)
          override def apply(
            registry: (H, RT),
            replacement: Version.Entry.Aux[A, R1, D]
          ): (H, RT1) =
            (registry._1, rest(registry._2, replacement))
        }
    }

    trait Build[Tpe, Reg] {
      def apply(ctrReg: Reg): Tpe
    }

    object Build {
      implicit val buildEmpty: Build[Unit, Unit] = new Build[Unit, Unit] {
        def apply(ctrReg: Unit): Unit = ctrReg
      }

      implicit def buildVersion[A, Tpe, Reg, RA](
        implicit rest: Build[Tpe, Reg]
      ): Build[(Schema[A], Tpe), (Version.Entry.Aux[A, RA, Tpe], Reg)] =
        new Build[(Schema[A], Tpe), (Version.Entry.Aux[A, RA, Tpe], Reg)] {

          def apply(
            ctrReg: (Version.Entry.Aux[A, RA, Tpe], Reg)
          ): (Schema[A], Tpe) = {
            val tail = rest(ctrReg._2)

            (ctrReg._1.entry(tail), tail)
          }
        }
    }

    trait Lookup[Re, A] {
      def apply(registry: Re): Schema[A]
    }

    trait LowPrioLookup {
      implicit def tailLookup[R1, RT, A](implicit rest: Lookup[RT, A]): Lookup[(R1, RT), A] =
        new Lookup[(R1, RT), A] {
          def apply(registry: (R1, RT)): Schema[A] = rest(registry._2)
        }
    }

    object Lookup extends LowPrioLookup {
      implicit def headLookup[RR, A]: Lookup[(Schema[A], RR), A] =
        new Lookup[(Schema[A], RR), A] {
          def apply(registry: (Schema[A], RR)): Schema[A] = registry._1
        }
    }

  }
 */
}
