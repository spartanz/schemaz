---
layout: docs
title: "Scalacheck Generators"
section: interpreters
---

# {{page.title}}

The `scalaz-schema-scalacheck` module provides interpreters for scalacheck random data generators (`org.scalacheck.Gen`).

## Example usage

```tut:silent
import scalaz._, schema._
import org.scalacheck._, Arbitrary._
import monocle.Iso
```

To use it, you'll need to define a module mixing the `scalaz.schema.scalacheck.GenModule` trait and to define an implicit `Prim ~> Gen` natural transformation from your specific set of primitives to `Gen`.

```tut:silent
object ExampleModule extends scalacheck.GenModule[JsonSchema.type] {
  
  val R = JsonSchema

  implicit val primToGen = new (R.Prim ~> Gen) {
    def apply[A](prim: R.Prim[A]): Gen[A] = prim match {
      case JsonSchema.JsonBool   => arbitrary[Boolean]
      case JsonSchema.JsonNumber => arbitrary[BigDecimal]
      case JsonSchema.JsonString => Gen.alphaNumStr
      case JsonSchema.JsonNull   => Gen.const(())
    }
  }
}
```

Using that module, we can then define a schema for some data structure.

```tut:silent
final case class Foo(name: String, connected: Boolean)

import ExampleModule._

val foo = record(
    "name"      -*>: prim(JsonSchema.JsonString) :*:
    "connected" -*>: prim(JsonSchema.JsonBool),
    Iso[(String, Boolean), Foo]((Foo.apply _).tupled)(f => (f.name, f.connected))
  )
  
```

And use that schema to derive a `Gen[Foo]`

```tut
val gen = foo.to[Gen]

gen.pureApply(Gen.Parameters.default, rng.Seed.random)
```
