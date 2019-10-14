package schemaz

trait Versioning[R <: Realisation] extends SchemaModule[R] {

  final val Current: Version[Unit, Unit] = new Version(())

  sealed case class Version[Types, Re](registry: Re)(
    implicit build: Version.Build[Types, Re]
  ) {

    lazy val types: Types = build(registry)

    def schema[A, RA, T](leaf: SchemaZ.Aux[RA, A, T])(
      implicit add: Version.AddEntry[Types, Unit, A, RA, T]
    ): Version[(SchemaZ[T], Types), (Version.Entry.Aux[A, T, RA, Types], Re)] =
      new Version(
        (add(Version.Entry((_: Unit) => leaf)), registry)
      )

    def schema[A, RA, D, T](ctr: D => SchemaZ.Aux[RA, A, T])(
      implicit add: Version.AddEntry[Types, D, A, RA, T]
    ): Version[(SchemaZ[T], Types), (Version.Entry.Aux[A, T, RA, Types], Re)] =
      new Version(
        (add(Version.Entry(ctr)), registry)
      )

    def schema[A, RA, D1, D2, T](ctr: (D1, D2) => SchemaZ.Aux[RA, A, T])(
      implicit add: Version.AddEntry[Types, (D1, D2), A, RA, T]
    ): Version[(SchemaZ[T], Types), (Version.Entry.Aux[A, T, RA, Types], Re)] =
      new Version(
        (add(Version.Entry(ctr.tupled)), registry)
      )

    def schema[A, RA, D1, D2, D3, T](ctr: (D1, D2, D3) => SchemaZ.Aux[RA, A, T])(
      implicit add: Version.AddEntry[Types, (D1, D2, D3), A, RA, T]
    ): Version[(SchemaZ[T], Types), (Version.Entry.Aux[A, T, RA, Types], Re)] =
      new Version(
        (add(Version.Entry(ctr.tupled)), registry)
      )

    def schema[A, RA, D1, D2, D3, D4, T](
      ctr: (D1, D2, D3, D4) => SchemaZ.Aux[RA, A, T]
    )(
      implicit add: Version.AddEntry[Types, (D1, D2, D3, D4), A, RA, T]
    ): Version[(SchemaZ[T], Types), (Version.Entry.Aux[A, T, RA, Types], Re)] =
      new Version(
        (add(Version.Entry(ctr.tupled)), registry)
      )

    def schema[A, RA, D1, D2, D3, D4, D5, T](
      ctr: (D1, D2, D3, D4, D5) => SchemaZ.Aux[RA, A, T]
    )(
      implicit add: Version.AddEntry[Types, (D1, D2, D3, D4, D5), A, RA, T]
    ): Version[(SchemaZ[T], Types), (Version.Entry.Aux[A, T, RA, Types], Re)] =
      new Version(
        (add(Version.Entry(ctr.tupled)), registry)
      )

    def migrate[T](
      implicit lookup: Version.LookupCtr[Re, T]
    ): Migration[Types, Re, lookup.A, lookup.Repr, lookup.Deps, T] =
      new Migration[Types, Re, lookup.A, lookup.Repr, lookup.Deps, T](registry, lookup)

    def lookup[T](implicit l: Version.Lookup[Types, T]): SchemaZ.Aux[l.Repr, l.A, T] = l(types)

  }

  final class Migration[Types, Re, A, RA, D, T](
    registry: Re,
    lookup: Version.LookupCtr.Aux[Re, A, RA, D, T]
  ) {

    def change[RA1, A1, Re1](migration: SchemaZ.Aux[RA, A, T] => SchemaZ.Aux[RA1, A1, T])(
      implicit replace: Version.Replace.Aux[Re, A, RA, A1, RA1, D, T, Re1],
      build: Version.Build[Types, Re1]
    ): Version[Types, Re1] = {
      val migrated = lookup(registry).post(migration)
      new Version[Types, Re1](replace(registry, migrated))
    }

    def replace[RA1, A1, Re1](migration: SchemaZ.Aux[RA, A, T] => SchemaZ.Aux[RA1, A1, T])(
      implicit replace: Version.Replace.Aux[Re, A, RA, A1, RA1, D, T, Re1]
    ) = {
      assert(migration != null)
      replace
    }
  }

  object Version {

    trait Entry[A, T] {
      type Deps
      type Repr
      val entry: Deps => SchemaZ.Aux[Repr, A, T]
      def pre[D0](f: D0 => Deps): Entry.Aux[A, T, Repr, D0]

      def post[R1, A1](
        f: SchemaZ.Aux[Repr, A, T] => SchemaZ.Aux[R1, A1, T]
      ): Entry.Aux[A1, T, R1, Deps]
    }

    object Entry {
      type Aux[A, T, RA, D] = Entry[A, T] { type Deps = D; type Repr = RA }

      def apply[A, T, RA, D](ctr: D => SchemaZ.Aux[RA, A, T]): Aux[A, T, RA, D] =
        VersionEntry[A, T, RA, D](ctr)
    }
    case class VersionEntry[A, T, RA, D](entry: D => SchemaZ.Aux[RA, A, T]) extends Entry[A, T] {
      type Deps = D
      type Repr = RA

      def pre[D0](f: D0 => Deps): Entry.Aux[A, T, Repr, D0] = Entry(entry.compose(f))

      def post[R1, A1](
        f: SchemaZ.Aux[Repr, A, T] => SchemaZ.Aux[R1, A1, T]
      ): Entry.Aux[A1, T, R1, Deps] =
        Entry(entry.andThen(f))

    }

    trait AddEntry[Re, D, A, RA, T] {
      def prepare: Re => D

      def apply(
        newEntry: Version.Entry.Aux[A, T, RA, D]
      ): Version.Entry.Aux[A, T, RA, Re] =
        newEntry.pre(prepare)
    }

    object AddEntry {

      implicit def noDependencies[Re, A, RA, T]: AddEntry[Re, Unit, A, RA, T] =
        new AddEntry[Re, Unit, A, RA, T] { def prepare: Re => Unit = _ => () }

      implicit def singleDependency[Re, D, A, RA, T](
        implicit D: Version.Lookup[Re, D]
      ): AddEntry[Re, SchemaZ[D], A, RA, T] = new AddEntry[Re, SchemaZ[D], A, RA, T] {
        def prepare: Re => SchemaZ[D] = (re => D(re))
      }

      implicit def twoDependencies[Re, D1, D2, A, RA, T](
        implicit D1: Version.Lookup[Re, D1],
        D2: Version.Lookup[Re, D2]
      ): AddEntry[Re, (SchemaZ[D1], SchemaZ[D2]), A, RA, T] =
        new AddEntry[Re, (SchemaZ[D1], SchemaZ[D2]), A, RA, T] {
          def prepare: Re => (SchemaZ[D1], SchemaZ[D2]) = (re => (D1(re), D2(re)))
        }

      implicit def threeDependencies[Re, D1, D2, D3, A, RA, T](
        implicit D1: Version.Lookup[Re, D1],
        D2: Version.Lookup[Re, D2],
        D3: Version.Lookup[Re, D3]
      ): AddEntry[Re, (SchemaZ[D1], SchemaZ[D2], SchemaZ[D3]), A, RA, T] =
        new AddEntry[Re, (SchemaZ[D1], SchemaZ[D2], SchemaZ[D3]), A, RA, T] {

          def prepare: Re => (SchemaZ[D1], SchemaZ[D2], SchemaZ[D3]) =
            (re => (D1(re), D2(re), D3(re)))
        }

      implicit def fourDependencies[Re, D1, D2, D3, D4, A, RA, T](
        implicit D1: Version.Lookup[Re, D1],
        D2: Version.Lookup[Re, D2],
        D3: Version.Lookup[Re, D3],
        D4: Version.Lookup[Re, D4]
      ): AddEntry[Re, (SchemaZ[D1], SchemaZ[D2], SchemaZ[D3], SchemaZ[D4]), A, RA, T] =
        new AddEntry[Re, (SchemaZ[D1], SchemaZ[D2], SchemaZ[D3], SchemaZ[D4]), A, RA, T] {

          def prepare: Re => (SchemaZ[D1], SchemaZ[D2], SchemaZ[D3], SchemaZ[D4]) =
            (re => (D1(re), D2(re), D3(re), D4(re)))
        }

      implicit def fiveDependencies[Re, D1, D2, D3, D4, D5, A, RA, T](
        implicit D1: Version.Lookup[Re, D1],
        D2: Version.Lookup[Re, D2],
        D3: Version.Lookup[Re, D3],
        D4: Version.Lookup[Re, D4],
        D5: Version.Lookup[Re, D5]
      ): AddEntry[
        Re,
        (SchemaZ[D1], SchemaZ[D2], SchemaZ[D3], SchemaZ[D4], SchemaZ[D5]),
        A,
        RA,
        T
      ] =
        new AddEntry[
          Re,
          (SchemaZ[D1], SchemaZ[D2], SchemaZ[D3], SchemaZ[D4], SchemaZ[D5]),
          A,
          RA,
          T
        ] {

          def prepare: Re => (SchemaZ[D1], SchemaZ[D2], SchemaZ[D3], SchemaZ[D4], SchemaZ[D5]) =
            (re => (D1(re), D2(re), D3(re), D4(re), D5(re)))
        }
    }

    trait LookupCtr[Re, T] {
      type Repr
      type Deps
      type A
      def apply(registry: Re): Version.Entry.Aux[A, T, Repr, Deps]
    }

    trait LowPrioLookupCtr {
      implicit def tailLookupCtr[R1, RT, A0, R0, D0, T](
        implicit rest: LookupCtr.Aux[RT, A0, R0, D0, T]
      ): LookupCtr.Aux[(R1, RT), A0, R0, D0, T] =
        new LookupCtr[(R1, RT), T] {

          type Repr = R0
          type Deps = D0
          type A    = A0
          def apply(registry: (R1, RT)): Version.Entry.Aux[A0, T, R0, D0] = rest(registry._2)
        }
    }

    object LookupCtr extends LowPrioLookupCtr {

      type Aux[Re, A0, R0, D0, T] = LookupCtr[Re, T] {
        type Repr = R0; type Deps = D0; type A = A0
      }

      implicit def headLookupCtr[RR, A0, R0, D, T]
        : LookupCtr.Aux[(Version.Entry.Aux[A0, T, R0, D], RR), A0, R0, D, T] =
        new LookupCtr[(Version.Entry.Aux[A0, T, R0, D], RR), T] {

          type Repr = R0
          type Deps = D
          type A    = A0

          def apply(
            registry: (Version.Entry.Aux[A0, T, R0, D], RR)
          ): Version.Entry.Aux[A0, T, R0, D] =
            registry._1
        }
    }

    trait Replace[Re, A0, R0, A1, R1, D, T] {
      type Out
      def apply(registry: Re, replacement: Version.Entry.Aux[A1, T, R1, D]): Out
    }

    object Replace {

      type Aux[Re, A0, R0, A1, R1, D, T, Re1] = Replace[Re, A0, R0, A1, R1, D, T] {
        type Out = Re1
      }

      implicit def tailReplace[A0, R0, A1, R1, D, RT, T]: Replace.Aux[
        (Version.Entry.Aux[A0, T, R0, D], RT),
        A0,
        R0,
        A1,
        R1,
        D,
        T,
        (Version.Entry.Aux[A1, T, R1, D], RT)
      ] =
        new Replace[(Version.Entry.Aux[A0, T, R0, D], RT), A0, R0, A1, R1, D, T] {
          type Out = (Version.Entry.Aux[A1, T, R1, D], RT)
          override def apply(
            registry: (Version.Entry.Aux[A0, T, R0, D], RT),
            replacement: Version.Entry.Aux[A1, T, R1, D]
          ): (Version.Entry.Aux[A1, T, R1, D], RT) = (replacement, registry._2)
        }

      implicit def headReplace[H, A0, R0, A1, R1, D, RT, RT1, T](
        implicit rest: Replace.Aux[RT, A0, R0, A1, R1, D, T, RT1]
      ): Replace.Aux[(H, RT), A0, R0, A1, R1, D, T, (H, RT1)] =
        new Replace[(H, RT), A0, R0, A1, R1, D, T] {
          type Out = (H, RT1)
          override def apply(
            registry: (H, RT),
            replacement: Version.Entry.Aux[A1, T, R1, D]
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

      implicit def buildVersion[A, T, Tpe, Reg, RA](
        implicit rest: Build[Tpe, Reg]
      ): Build[(SchemaZ[T], Tpe), (Version.Entry.Aux[A, T, RA, Tpe], Reg)] =
        new Build[(SchemaZ[T], Tpe), (Version.Entry.Aux[A, T, RA, Tpe], Reg)] {

          def apply(
            ctrReg: (Version.Entry.Aux[A, T, RA, Tpe], Reg)
          ): (SchemaZ[T], Tpe) = {
            val tail = rest(ctrReg._2)

            (ctrReg._1.entry(tail), tail)
          }
        }
    }

    trait Lookup[Re, T] {

      type A
      type Repr
      def apply(registry: Re): SchemaZ.Aux[Repr, A, T]
    }

    trait LowPrioLookup {
      implicit def tailLookup[R1, RT, T](
        implicit rest: Lookup[RT, T]
      ): Lookup.Aux[(R1, RT), T, rest.Repr, rest.A] =
        new Lookup[(R1, RT), T] {

          type A    = rest.A
          type Repr = rest.Repr
          def apply(registry: (R1, RT)): SchemaZ.Aux[rest.Repr, rest.A, T] = rest(registry._2)
        }
    }

    object Lookup extends LowPrioLookup {

      type Aux[Re, T, R0, A0] = Lookup[Re, T] {
        type A = A0; type Repr = R0
      }

      implicit def headLookup[RR, R0, A0, T]: Lookup.Aux[(SchemaZ.Aux[R0, A0, T], RR), T, R0, A0] =
        new Lookup[(SchemaZ.Aux[R0, A0, T], RR), T] {

          type A    = A0
          type Repr = R0

          def apply(registry: (SchemaZ.Aux[R0, A0, T], RR)): SchemaZ.Aux[R0, A0, T] =
            registry._1
        }
    }

  }

}
