package schemaz

import scalaz.~>

package recursion {

  trait HFunctor[H[_[_], _]] {
    def hmap[F[_], G[_]](nt: F ~> G): H[F, ?] ~> H[G, ?]
  }

  final case class Fix[F[_[_], _], A](unFix: F[Fix[F, ?], A])

  final case class HEnvT[E, F[_[_], _], G[_], I](ask: E, fa: F[G, I])

  object HEnvT {

    implicit def hfunctor[E, F[_[_], _]](implicit F: HFunctor[F]): HFunctor[HEnvT[E, F, ?[_], ?]] =
      new HFunctor[HEnvT[E, F, ?[_], ?]] {

        def hmap[M[_], N[_]](nt: M ~> N) = new (HEnvT[E, F, M, ?] ~> HEnvT[E, F, N, ?]) {
          def apply[I](fm: HEnvT[E, F, M, I]) = HEnvT(fm.ask, F.hmap(nt)(fm.fa))
        }
      }
  }
}

package object recursion {
  type HAlgebra[F[_[_], _], G[_]]   = F[G, ?] ~> G
  type HCoalgebra[F[_[_], _], G[_]] = G ~> F[G, ?]

  def cataNT[S[_[_], _], F[_]](
    alg: HAlgebra[S, F]
  )(implicit S: HFunctor[S]): (Fix[S, ?] ~> F) =
    new (Fix[S, ?] ~> F) { self =>

      def apply[A](f: Fix[S, A]): F[A] =
        alg.apply[A](S.hmap(self)(f.unFix))
    }

  def hyloNT[S[_[_], _], F[_], G[_]](coalgebra: HCoalgebra[S, F], algebra: HAlgebra[S, G])(
    implicit S: HFunctor[S]
  ): F ~> G = new (F ~> G) { self =>

    def apply[A](fa: F[A]): G[A] =
      algebra(S.hmap(self)(coalgebra(fa)))
  }

}
