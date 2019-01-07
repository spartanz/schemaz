---
layout: docs
title: "Interpreters"
section: interpreters
position: 2
---

# {{page.title}}

Given a schema for a type `A`, we can derive operations on `A` from it. Intuitively, these operations are functions that take an `A` as argument, or return an `A`, or both. But this can be generalised as "some functor `F` acting on `A`". 

In other words, interpreting schemas into a given functor `F` is coming up with a function from `Schema[X]` to `F[X]` for any type `X`. Such functions are called *natural transformations* and denoted `Schema ~> F` using scalaz.

## HAlgebras

Thanks to the recursive nature of the schema GADT, these interpreters are produced by *folding* a schema using an *algebra* (to use the terminology of recursion schemes). In general, an algebra for a functor `M` and *carrier* `A` is just a function `M[A] => A`. 

But here things are slightly more complicated since `Schema` isn't a mere functor, but an *higher-order* functor. That is, a functor acting on functors.

```scala
sealed trait Schema[F[_], A]
```

So our algebras cannot be simple functions of shape `Schema[A] => A`, they must be natural transformations of shape `Schema[F, ?] ~> F` (with `F` as the carrier). Generalizing to any higher-order functor `S` this would be defined as: 

```scala
type HAlgebra[S[_[_], _], F[_]] = S[F, ?] ~> F
```

This can look a bit scary at first, but implementing such algebra is in fact quite straightforward. Assuming some `Target` functor, we just write:

```scala
val alg: HAlgebra[Schema, Target] = new (Schema[Target, ?] ~> Target) {
  def apply[A](schema: Schema[Target, A]): Target[A] = schema match {
    case One()     => ???
    case :+:(l, r) => ???
    case :*:(l, r) => ???
    // etc.
  }
}
```

If you look hard enough at the type signatures, you'll realize that pattern variables `l` and `r` must have type `Target[x]` and `Target[y]`, for some types `x` and `y`, respectively. That is to say that within an algebra, you process nodes whose children (if any) have already been folded into your target functor; all you have to do is to combine these partial results accordingly. For instance, in the case of a `:+:` node, we just need to combine a `Target[x]` and a `Target[y]` into a `Target[x \/ y]`.

## Using an interpreter

Once you have implemented an `Halgebra` for a target functor `F`, you can use the `Schema.cataNT` method to fold a schema with it:

```scala
Schema.cataNT(mySchema, myAlgebra)
```

Moreover, whenever such an `Halgebra[Schema, F]` is available in the implicit scope, you can use the `to` extension method on your schema to interpret it into an `F`:

```scala
mySchema.to[F]
```

## Fix

TODO

## Interpreter modules

For most applications though, you won't need to write interpreters by yourself. Scalaz-Schema offers modules providing interpreters for target functors covering a wide variety of use cases: `org.scalacheck.Gen`, encoders and decoders for widely used data formats (Avro, protobuf, etc.) or popular JSON AST libraries (play-json, circe, etc.). Every such module is listed in the remaining of this section.

The provided interpreters abstract over the "representation" details of your schema. More specifically, because `Prim`, `ProductTermId` and `SumTermId` are abstract in `SchemaModule`, interpreter modules must be told how to concretely handle primitives and fields/branches labels in your specific context before giving you a concrete interpreter for that context. 

Concretely, you will have to provide a `Prim ~> F` natural transformation that will interpret your set of primitives into the module's target functor `F`. Most modules (but not all of them) will also require a proof that your `SumTermId` and `ProductTermId` can be converted to some other type. For example, a module that provides JSON-related operations will require that both can be converted to `String`.
 
In general, you will have to provide these prerequisites as implicit values, unless specified otherwise in a module's documentation. Doing so will enable a module's implicit method returning an `HAlgebra[Schema, F]` which will in turn allow you to call `to[F]` on your schemas.

A full example is shown in the [scalacheck module documentation](scalacheck.html).
