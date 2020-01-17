package schemaz

object Representation {
  type RSum[RA, A, RB, B]
  type RProd[RA, A, RB, B]
  type RSelf[A]
  type RSeq[R, A]
  type -*>[K, V]
  type -+>[K, V]
  type RRecord[RA, A]
  type RUnion[RA, A]
}
