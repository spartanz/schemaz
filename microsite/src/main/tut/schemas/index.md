---
layout: docs
title: "Schemas"
section: schemas
position: 1
---

# {{page.title}}

Schemas are values that represent the structure of a given piece of data. We could say that schemas are to runtime what types are to compile time.

In theory every *algebraic* data type can be repesented using only four constructs:

* **Unit**: the schema for a type that as exactly one inhabitant.
* **Sum**: given the schemas for two types `A` and `B`, their sum is the schema for a type that is inhabited by the union of the inhabitants of `A` and the inhabitants of `B`. In other words, it's the schema for `A \/ B` (or `Either[A, B]`). 
* **Product**: given the schemas for two types `A` and `B`, their product is the schema for a type that is inhabited by the cartesian product of the inhabitants of `A` and the inhabitants of `B`. In other words, it's the schema for `(A, B)`.
* **Recursion**: the ability for a schema to be defined in terms of itself.

For example, we can represent `Boolean` as `Unit \/ Unit` (picking `-\/(())` as `true` and `\/-(())` as `false`), `Byte` as `(Unit \/ Unit, Unit \/ Unit, Unit \/ Unit, Unit \/ Unit, Unit \/ Unit, Unit \/ Unit, Unit \/ Unit, Unit \/ Unit)` (the product of eight `Boolean`s representing a bit each), and so on.

Even higher-order constructs can be represented that way. `Option[A]` can be seen as `A \/ Unit` (that is: all the inhabitants of `A` plus one to represent `None`) and `List[A]` as `Unit \/ (A, List[A])` (ie. either `Nil` or the product of an element of type `A` — the head — and a `List[A]` — the tail —, notice the recursion).

This is in fact the very reason why these structures are called Algebraic Data Types: they are the productions of the algebra of unit, sum, product and recursion. We therefore call schemas composed only of unit, sum, product and recursion *essential schemas*.

But of course, this encoding isn't very convenient: it is much shorter to write `Long` than `(Unit \/ Unit, ..., Unit \/ Unit)` (with 62 repetitions of `Unit \/ Unit` instead of `...`). We are also used to work with *records* (aka case classes) that have named *fields* instead of mere products (aka tuples) and with *unions* (aka sealed traits) that have named *branches* instead of mere sums (aka `\/` or `Either`).

To account for these usability issues, the library provides:
* a way to define *primitive types*, allowing forn instance to use `Boolean` instead of `Unit \/ Unit`
* *records* allowing to represent case classes in terms of a "product of named fields"
* *unions* allowing to represent sealed trait in terms of a "sum of named branches"

Finally, if we consider that the purpose of a schema for a type `A` is to derive operations on values of type `A` (ie. functions that take an `A` as argument, or return an `A`, or both), then given an isomorphism between `A` and some other type `B`, we can translate all the operations acting on `A` we derive from a schema for `A` to a corresponding operation acting on `B`. These translated operation would be the same as those we would have derived directly from a schema for `B`. In other words, having a schema for `A` and an isomorphism between `A` and `B` is exactly the same as having a schema for `B`.

That's why the library provides a way to build a schema for `B` out of a schema for `A` and a `monocle.Iso[A, B]`. In fact, *convenience schemas* (ie. records and unions) are implemented this way, in terms of an `Iso` mapped onto an *essential schema* (ie. a product or a sum).

## Schema modules

The Schema API abstracts over how these *convenience schemas* are effectively represented. More specifically, it leaves abstract the following types:
* `Prim[A]`: the set of *primitive* types, implemented as an ADT.
* `SumTermId`: the type used to label *branches* of an *union*.
* `ProductTermId`: the type used to label *fields* of a *record*.

So in order to build a schema, you must first define a concrete `SchemaModule` implementing these types. For example, if we are dealing with schemas for JSON documents, we can write:

```tut:silent
import scalaz._, schema._
```

```tut:silent
object JsonRealisation extends Realisation {
  type Prim[A] = JsonPrim[A]
  type SumTermId = String
  type ProductTermId = String

  sealed trait JsonPrim[A]
  case object JsonBoolean extends JsonPrim[Boolean]
  case object JsonNumber extends JsonPrim[BigDecimal]
  case object JsonString extends JsonPrim[String]
  case object JsonNull extends JsonPrim[Unit]
}

object JsonModule extends SchemaModule[JsonRealisation.type] {
  val R = JsonRealisation
}
```

We will later use this module to build schemas.

## Building schemas

```tut:silent
import JsonModule._
import JsonRealisation._
```

Our newly created module provides combinators to build schemas. 

The simplest one is `unit`, it gives you the schema for `Unit`.

```tut
unit
```

We'll talk about the `FSchema` result type and this `Fix` thingy later when we explain how to interpret a schema. For now you just need to understand `FSchema[A]` as "the schema for type `A`". 

To define sums, we use the `:+:` combinator. 

```tut
val bit = unit :+: unit
```

And to define products, we use `:*:`

```tut
val u8 = bit :*: bit :*: bit :*: bit :*: bit :*: bit :*: bit :*: bit
```

What if we wanted to use the representation described by the `bit` schema to represent `Boolean` values? Using `iso`, we can account for the fact that `Boolean` and `Unit \/ Unit` are isomorphic and express `boolean` in terms of `bit`:

```tut
import monocle.Iso

val bitToBoolean = Iso[Unit \/ Unit, Boolean]{
  case -\/(()) => true
  case \/-(()) => false
}{
  case true => -\/(())
  case false => \/-(())
}

val boolean = iso(bit, bitToBoolean)
```

But representing booleans this way isn't really efficient since we are in a module that has a boolean primitive. So we'd rather use `prim` to build a schema using that primitive:

```tut
val boolean = prim(JsonBoolean)
```

Alright, that's all fun and games, but in real-world applications, we need to represent algebraic data types like the following:

```tut:silent
sealed trait Message
case class Greeting(from: String, to: String) extends Message 
case object Bye                               extends Message
```

To build a schema for the whole `Message` ADT, we would of course need to build the schema for all its sub-types. So let's start by building the schema for `Greeting`. 

`Greeting` is a case class, so we represent it as a *record*: a (nested) product of labelled *fields*, which are just schemas with a `ProductTermId` attached to them (in our current module, `ProductTermId` is `String`). The `-*>:` combinator is there for attaching such label to a schema.

To build a record from a product of labelled fields, we also need to provide an isomorphism between the product representation and the concrete case class our record is meant to represent. In the case of `Greeting`, we need an `Iso[(String, String), Greeting]`.

```tut
val tupleToGreeting = Iso[(String, String), Greeting]((Greeting.apply _).tupled)(g => (g.from, g.to))

val greeting = record(
  "from" -*>: prim(JsonString) :*:
  "to"   -*>: prim(JsonString),
  tupleToGreeting
)
```

The schema for `Bye` is much simpler. `Bye` is a case object that is, a singleton type, a type with only one inhabitant. We already have a schema for a type with only one inhabitant, it's `unit`. So we just have to prove that `Bye` and `Unit` are isomorphic:

```tut
val bye = iso(unit, Iso[Unit, Bye.type](_ => Bye)(_ => ()))
```

And finally, we can define the schema for `Message` as the union (sum of labelled branches) of `greeting` and `bye`. We use `-+>:` to label the branches of an union. As with records, we need to provide an isomorphism between the sum representation (`Greeting \/ Byt.type`) and the `Message` trait:

```tut
val sumToMessage = Iso[Greeting \/ Bye.type, Message]{
  case -\/(g) => g
  case \/-(b) => b
}{
  case g @ Greeting(_, _) => -\/(g)
  case Bye                => \/-(Bye)
}

val message = union(
  "Greeting" -+>: greeting :+:
  "Bye"      -+>: bye,
  sumToMessage
)
```

And *voilà*! We have built a schema for our `Message` ADT.

The library also provides combinator to represent lists (`seq`)  and options (`optional`):

```tut
val messageList = seq(message)

val maybeMessage = optional(message)
```




