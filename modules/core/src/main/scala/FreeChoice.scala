package scalaz

package schema

sealed trait FreeChoice[F[_], A]

object FreeChoice {

  def fold[F[_], G[_]: Thing, A](choice: FreeChoice[F, A])(nt: F ~> G): G[A] = choice match {
    case e: End[F, at] => nt(e.fa)
    //in this case A has to be Either[at, bt]
    case b: ChoiceBranch[F, at, bt] => Thing[G].choose(nt(b.fa), fold(b.tail)(nt))
  }

  implicit class FreeChoiceOps[F[_], B](choices: FreeChoice[F, B]) {
    def :: [A](fa: F[A]): FreeChoice[F, Either[A, B]] = ChoiceBranch(fa, choices)
  }
}

final case class ChoiceBranch[F[_], A, B](fa: F[A], tail: FreeChoice[F, B])
    extends FreeChoice[F, Either[A, B]]
final case class End[F[_], A](fa: F[A]) extends FreeChoice[F, A]
