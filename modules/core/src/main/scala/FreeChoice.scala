package scalaz

package schema

sealed trait FreeChoice[F[_], A]

object FreeChoice {

  def covariantFold[F[_], G[_]: Alt, A](choice: FreeChoice[F, A])(nt: F ~> G): G[A] = choice match {
    case e: End[F, at] => nt(e.fa)
    //in this case A has to be Either[at, bt]
    case b: ChoiceBranch[F, at, bt] => Alt[G].either2(nt(b.fa), covariantFold(b.tail)(nt))
  }

  def contravariantFold[F[_], G[_]: Decidable, A](choice: FreeChoice[F, A])(nt: F ~> G): G[A] =
    choice match {
      case e: End[F, at] => nt(e.fa)
      //in this case A has to be Either[at, bt]
      case b: ChoiceBranch[F, at, bt] =>
        Decidable[G].choose2(nt(b.fa), contravariantFold(b.tail)(nt))(identity)
    }

  implicit class FreeChoiceOps[F[_], B](choices: FreeChoice[F, B]) {
    def :: [A](fa: F[A]): FreeChoice[F, A \/ B] = ChoiceBranch(fa, choices)
  }

  implicit def functionDecidable[X: Monoid]: Decidable[? => X] = new Decidable[? => X] {
    override def choose2[Z, A1, A2](a1: => (A1 => X), a2: => (A2 => X))(
      f: Z => A1 \/ A2
    ): (Z => X) = z => f(z).fold(a1, a2)

    override def conquer[A]: A => X = _ => Monoid[X].zero

    override def divide[A, B, C](fa: A => X, fb: B => X)(f: C => (A, B)): C => X = c => {
      val tpl = f(c)
      Monoid[X].append(fa(tpl._1), fb(tpl._2))
    }
  }
}

final case class ChoiceBranch[F[_], A, B](fa: F[A], tail: FreeChoice[F, B])
    extends FreeChoice[F, A \/ B]
final case class End[F[_], A](fa: F[A]) extends FreeChoice[F, A]
