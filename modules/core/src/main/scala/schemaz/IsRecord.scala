package schemaz

import Representation.{ -*>, RProd }

@annotation.implicitNotFound(
  msg = "It seems like the following representation type isn't isomorphic to a product of named fields: ${A}"
)
trait IsRecord[A]

object IsRecord {
  implicit def singleFieldIsRecord[K, V]: IsRecord[K -*> V] = new IsRecord[K -*> V] {}

  implicit def productIsRecord[L: IsRecord, R: IsRecord, X, Y]: IsRecord[RProd[L, X, R, Y]] =
    new IsRecord[RProd[L, X, R, Y]] {}
}
