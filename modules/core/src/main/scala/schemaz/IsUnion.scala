package schemaz

import Representation.{ -+>, RSum }

@annotation.implicitNotFound(
  msg = "It seems like the following representation type isn't isomorphic to a sum of named branches: ${A}"
)
trait IsUnion[A]

object IsUnion {

  implicit def singleBranchIsUnion[K, V]: IsUnion[K -+> V] = new IsUnion[K -+> V] {}

  implicit def sumIsUnion[L: IsUnion, R: IsUnion, X, Y]: IsUnion[RSum[L, X, R, Y]] =
    new IsUnion[RSum[L, X, R, Y]] {}
}
