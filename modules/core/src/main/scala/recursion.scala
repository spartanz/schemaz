package schemaz

import scalaz.{ ~~> }

package recursion {

  trait HFunctor[H[_[_, _], _, _]] {
    def hmap[F[_, _], G[_, _]](nt: F ~~> G): H[F, ?, ?] ~~> H[G, ?, ?]
  }

  final case class Fix[F[_[_, _], _, _], R, A](unFix: F[Fix[F, ?, ?], R, A])

  final case class HEnvT[E, F[_[_, _], _, _], G[_, _], R, I](ask: E, fa: F[G, R, I])

  object HEnvT {

    implicit def hfunctor[E, F[_[_, _], _, _]](
      implicit F: HFunctor[F]
    ): HFunctor[HEnvT[E, F, ?[_, _], ?, ?]] =
      new HFunctor[HEnvT[E, F, ?[_, _], ?, ?]] {

        def hmap[M[_, _], N[_, _]](nt: M ~~> N) =
          new (HEnvT[E, F, M, ?, ?] ~~> HEnvT[E, F, N, ?, ?]) {
            def apply[R, I](fm: HEnvT[E, F, M, R, I]) = HEnvT(fm.ask, F.hmap(nt)(fm.fa))
          }
      }
  }
}

package object recursion {
  type HAlgebra[F[_[_, _], _, _], G[_, _]]   = F[G, ?, ?] ~~> G
  type HCoalgebra[F[_[_, _], _, _], G[_, _]] = G ~~> F[G, ?, ?]

  def cataNT[S[_[_, _], _, _], F[_, _]](
    alg: HAlgebra[S, F]
  )(implicit S: HFunctor[S]): (Fix[S, ?, ?] ~~> F) =
    new (Fix[S, ?, ?] ~~> F) { self =>

      def apply[R, A](f: Fix[S, R, A]): F[R, A] =
        alg.apply[R, A](S.hmap(self)(f.unFix))
    }

  def hyloNT[S[_[_, _], _, _], F[_, _], G[_, _]](
    coalgebra: HCoalgebra[S, F],
    algebra: HAlgebra[S, G]
  )(
    implicit S: HFunctor[S]
  ): F ~~> G = new (F ~~> G) { self =>

    def apply[R, A](fa: F[R, A]): G[R, A] =
      algebra(S.hmap(self)(coalgebra(fa)))
  }

}
