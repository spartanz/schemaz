package scalaz

package schema

sealed trait FreeAp2[F[_], A]
final case class FPure[F[_], A](fa: F[A])                       extends FreeAp2[F, A]
final case class FAp[F[_], A, B](fa: F[A], tail: FreeAp2[F, B]) extends FreeAp2[F, (A, B)]

object FreeAp2 {

  def covariantFold[F[_], G[_]: Applicative, A](fa: FreeAp2[F, A])(nt: F ~> G): G[A] = fa match {
    case FPure(fa)          => nt(fa)
    case ap: FAp[F, at, bt] => Applicative[G].tuple2(nt(ap.fa), covariantFold(ap.tail)(nt))
  }

  def contravariantFold[F[_], G[_]: Divisible, A](fa: FreeAp2[F, A])(nt: F ~> G): G[A] = fa match {
    case FPure(fa) => nt(fa)
    case ap: FAp[F, at, bt] =>
      Divisible[G].divide2(nt(ap.fa), contravariantFold(ap.tail)(nt))(identity)
  }

  implicit class FreeAp2Ops[F[_], B](products: FreeAp2[F, B]) {
    def :: [A](fa: F[A]): FreeAp2[F, (A, B)] = FAp(fa, products)
  }

  implicit def functionDivisible[X: Monoid]: Divisible[? => X] = new Divisible[? => X] {
    def conquer[A]: A => X = _ => Monoid[X].zero

    def contramap[A, B](r: A => X)(f: B => A): B => X = f.andThen(r)

    def divide[A, B, C](fa: A => X, fb: B => X)(f: C => (A, B)): C => X = c => {
      val tpl = f(c)
      Monoid[X].append(fa(tpl._1), fb(tpl._2))
    }

  }

}
