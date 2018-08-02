package scalaz

package schema

package scalacheck

trait GenModule extends SchemaModule {
  // def gen[A](schema: Schema[A])(implicit arb: Arbitrary[Prim[_]]): Gen[A]
}
