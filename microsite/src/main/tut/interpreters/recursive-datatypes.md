---
layout: docs
title: "Recursive data types"
section: interpreters
position: 2
---

# {{ page.title }}

It is quite easy to define schemas for recursive data types, but extra care must be taken when it comes to derive functors from such schemas. In fact, there is possibility of non-termination at two distinct levels:
* when building the interpreter
* when "running" the resulting functor


## Building interpreters

The construction of an interpreter is guarantied to terminate under certain conditions. The `SelfReference` case of the `SchemaF` ADT has a lazy `unroll` field that "performs the recursion". Evaluating this field eagerly in an algebra will make the interpretation diverge, but keeping it unevaluated will guarantee the interpretation to terminate.

For example, if we were to write an interpreter for `org.scalacheck.Gen` (that is, to implement an `HAlgebra[Schema, Gen]`), writing the following makes the process diverge:

```scala
// ...
  case ref @ SelfReference(_, _) => ref.unroll
// ...
```

The evaluation of `ref.unroll` causes the application of a natural transformation to the (self-) referenced schema. This natural transformation is the composition of several "smaller" natural transformations *including the algebra we are defining here*. This causes `ref.unroll` being evaluated over and over until the stack blows up.

That's why **`ref.unroll` should never be evaluated within an algebra**.

But if we keep the `unroll` field unevaluated, by passing it as a by-name parameter to some other function or by putting it in the body of an anonymous function, we are safe:

```scala
// ...
  case ref @ SelfReference(_, _) => Gen.delay(ref.unroll)
//...
```

## Non-termination of the derived functor

It is impossible to guarantee non-termination of a derived functor in general, but here are a few tips.

* Contravariant functors resembling to a function `A => X` (like, for example, `play.api.libs.json.Writes`) are safe as long as the input data is acyclic.
* Covariant functors resembling to a function `X => A` (like `play.api.libs.json.Reads`) are generaly safe, as long as you can prove that "each step consumes a bit of the `X`".
* Covariant functors resembling to a function `() => A` (like `org.scalacheck.Gen`) are unsafe, unless they provide some "delay mechanism".
