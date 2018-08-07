# scalaz-schema

[![Gitter](https://badges.gitter.im/scalaz/scalaz-schema.svg)](https://gitter.im/scalaz/scalaz-schema?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


## Goal

A purely-functional library for defining type-safe schemas for algebraic data types, providing free generators, SQL queries, JSON codecs, binary codecs, and migration from this schema definition.

## Introduction & Highlights

Scalaz Schema defines a generic representation of algebraic data structures and combinators that turn such schema into a generic computation over arbitrary data. In other words, Scalaz Schema provides a way to express any computation that abstracts over the structure of data, such as:

* Codecs: given a serial format (binary, JSON, etc.) and the schema of a data structure (say, an ADT) we can derive a codec for that data structure and serial format.
* Data Generators: given any schema we can derive random data generators (eg. scalacheck's `Gen`) that produce data satisfying that schema.
* Schema/Data Migrations: since schemas are values, we can easily verify whether two versions of a schema are forward/backward compatible and provide a generic way to upgrade/downgrade data from one version of the schema to the other.
* Diffing/Patching: given a schema we can generically compute the difference between two pieces of data satisfying that schema. In the same spirit, we have generic way to apply patches to arbitrary data structures.
* Queries: knowing a schema, we can produce SQL queries to interact with a database that holds an instance of (the SQL version of) that schema.


## Competition


 | | codecs | generators | migrations | diff/patch | queries | 
 ---|---|---|---|---|---
 xenomorph | ✓ | ✓ | ? | 𐄂 | 𐄂
 shapeless * | ✓ | ✓ | 𐄂 | ✓ | 𐄂
 
 \*: shapeless provides only the way to abstract over the structure of data, but several libraries build upon shapeless to provide the feature listed in the table.
 
## Background

Scalaz Schema shares ideas with @nuttycom's [xenomorph](https://github.com/nuttycom/xenomorph) library. The talk below presents its design.

<a href="http://www.youtube.com/watch?feature=player_embedded&v=oRLkb6mqvVM" target="_blank"><img src="http://img.youtube.com/vi/oRLkb6mqvVM/0.jpg" 
alt="Describing Data...with free applicative functors (and more)—Kris Nuttycombe" width="240" height="180" border="10" /></a>
