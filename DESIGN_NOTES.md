# scalaz-schema design notes
This document aims to lay out explanations for every scalaz-schema major design change.


## Free Applicative for record fields

At first we defined record schema as nonempty list of record fields that looked like:
```
sealed case class RecordSchema[A](terms: NonEmptyList[Field[A, _]]) extends Schema[A]
```
During development of `GenModule` we discovered that definition  was not expressive enough.
Imagine that you have person record that looks like:
```
case class Person(name: String, age: Int)
```
and you want to generate `Gen[Person]` out of your `Schema[Person]`.
If you do it by hand first thing what you would do is create gens for person fields,
`Gen[String]` and `Gen[Int]`.
Now if `Gen` is applicative it is relatively easy to create `Gen[Person]` out of gens for fields.
You could create it if `Gen` is monad but applicative fits here naturally since you don't need sequential
computation to create this object. You are done now.

But what to do if you want to have generic implementation?
By just looking at record schema for type `A` and list of it fields you don't know how to create `A`
if you have values of its fields. That is why we use free applicative that has fields `Field[A, A0]`
for its algebra and produces `A`:
```
sealed case class RecordSchema[A](fields: FreeAp[Field[A, ?], A]) extends Schema[A]
```

If you have free applicative you can easily interpret it to some other type constructor
`F[_]` if `F` is applicative using `foldMap` method that has signature:
```
  def foldMap[G[_]:Applicative](f: F ~> G): G[A]
```
That is what we did in [Gen module](https://github.com/scalaz/scalaz-schema/pull/19/files#diff-b36b3e5424683afebef8984ee076d92fR35).
We defined applicative for gen, natural transformation from field algebra to gen and used foldMap to get gen for record type.
