package scalaz

package schema

package recursion {

  trait HFunctor[H[_[_], _]] {
    def hmap[F[_], G[_]](nt: F ~> G): H[F, ?] ~> H[G, ?]
  }

  trait HTraverse[F[_[_], _]] extends HFunctor[F] {
    /*
    def traverse[G[_]: Applicative, A[_], B[_]](
      f: A ~> λ[α => G[B[α]]]
    ): F[A, ?] ~> λ[α => G[F[B, α]]]
     */
    def sequence[G[_]: Applicative, A[_]]: F[λ[α => G[A[α]]], ?] ~> λ[α => G[F[A, α]]]
  }

  trait HMonad[F[_[_], _]] {
    def hBind[G[_], H[_]](nt: G ~> F[H, ?]): F[G, ?] ~> F[H, ?]
    def hPure[G[_]]: G ~> F[G, ?]
  }

  sealed trait HState[S[_], F[_], A] {
    def run(sa: S[A]): F[A]
  }

  object HState {
    implicit def hMonadInstance[S[_]]: HMonad[HState[S, ?[_], ?]] =
      new HMonad[HState[S, ?[_], ?]] {

        override def hPure[G[_]]: G ~> HState[S, G, ?] = new (G ~> HState[S, G, ?]) {
          def apply[X](gx: G[X]): HState[S, G, X] = HStatePure(gx)
        }

        override def hBind[G[_], H[_]](
          nt: G ~> HState[S, H, ?]
        ): HState[S, G, ?] ~> HState[S, H, ?] = new (HState[S, G, ?] ~> HState[S, H, ?]) {

          def apply[X](hsg: HState[S, G, X]): HState[S, H, X] = hsg match {
            case HStatePure(ga) => nt(ga)
            case HStateNT(runNT) =>
              HStateNT(
                new (S ~> H) {
                  def apply[Y](sy: S[Y]): H[Y] = runNT.andThen(nt).apply(sy).run(sy)
                }
              )
          }
        }
      }

  }

  case class HStatePure[S[_], F[_], A](fa: F[A]) extends HState[S, F, A] {
    override def run(sa: S[A]): F[A] = fa
  }

  case class HStateNT[S[_], F[_], A](runNT: S ~> F) extends HState[S, F, A] {
    override def run(sa: S[A]): F[A] = runNT(sa)
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
  type HAlgebra[F[_[_], _], G[_]]                 = F[G, ?] ~> G
  type HCoalgebra[F[_[_], _], G[_]]               = G ~> F[G, ?]
  type HAlgebraM[M[_], F[_[_], _], G[_]]          = F[G, ?] ~> λ[α => M[G[α]]]
  type HCoalgebraM[M[_], F[_[_], _], G[_]]        = G ~> λ[α => M[F[G, α]]]
  type HAlgebraHM[M[_[_], _], F[_[_], _], G[_]]   = F[G, ?] ~> M[G, ?]
  type HCoalgebraHM[M[_[_], _], F[_[_], _], G[_]] = G ~> M[F[G, ?], ?]

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

  def hyloHM[M[_[_], _], S[_[_], _], F[_], G[_]](
    coalgebra: HCoalgebraHM[M, S, F],
    algebra: HAlgebraHM[M, S, G]
  )(implicit M: HMonad[M], S: HTraverse[S]) = ???

  def hyloM[M[_], S[_[_], _], F[_], G[_]](
    coalgebra: HCoalgebraM[M, S, F],
    algebra: HAlgebraM[M, S, G]
  )(implicit M: Monad[M], S: HTraverse[S]): F ~> λ[α => M[G[α]]] = {
    type MS[X[_], A] = M[S[X, A]]
    type MG[A]       = M[G[A]]
    hyloNT[MS, F, MG](coalgebra, new (MS[MG, ?] ~> MG) {

      def apply[A](msmga: M[S[MG, A]]): MG[A] =
        M.bind(msmga)(smga => M.bind(S.sequence[M, G].apply(smga))(algebra.apply[A]))

    })(new HFunctor[MS] {

      def hmap[A[_], B[_]](f: A ~> B) = new (MS[A, ?] ~> MS[B, ?]) {
        def apply[X](msax: MS[A, X]): MS[B, X] =
          M.map(msax)(S.hmap(f))
      }

    })

  }

}
