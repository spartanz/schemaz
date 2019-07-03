---
layout: home
title: "Home"
---

# Welcome to SchemaZ

SchemaZ is a purely functional library that provides means for defining runtime desciptions of data structures (aka schemas), and lets you derive various computations over the corresponding data, for free.

For example, given the schema for an arbitrary Scala ADT, we can derive JSON codecs that would convert JSON documents to the ADT (back and forth) or random generators for property-based testing (eg. `org.scalacheck.Gen`) and so on.

In theory, any treatment that depends only on the data's *structure* can be derived from its schema.


