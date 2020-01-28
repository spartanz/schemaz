package schemaz.recursion

import scalaz.~>

/**
 * An interpreter able to derive a `F[A]` from a schema for `A` (for any `A`).
 * Such interpreters will usually be implemented using a recursion scheme like
 * 'cataNT`or hyloNT`.
 */
trait Interpreter[F[_], G[_]] { self =>

  /**
   * A natural transformation that will transform a schema for any type `A`
   * into an `F[A]`.
   */
  def interpret: F ~> G

  def compose[H[_]](nt: H ~> F) = self match {
    case i: ComposedInterpreter[h, G, F] => ComposedInterpreter(i.underlying, i.nt.compose(nt))
    case x                               => ComposedInterpreter(x, nt)
  }
}

final case class ComposedInterpreter[F[_], G[_], H[_]](
  underlying: Interpreter[F, G],
  nt: H ~> F
) extends Interpreter[H, G] {
  final override val interpret = underlying.interpret.compose(nt)
}

class CataInterpreter[S[_[_], _], F[_]](
  algebra: HAlgebra[S, F]
)(implicit ev: HFunctor[S])
    extends Interpreter[Fix[S, ?], F] {
  final override val interpret = cataNT(algebra)
}

class HyloInterpreter[S[_[_], _], F[_], G[_]](
  coalgebra: HCoalgebra[S, G],
  algebra: HAlgebra[S, F]
)(implicit ev: HFunctor[S])
    extends Interpreter[G, F] {
  final override val interpret = hyloNT(coalgebra, algebra)
}

object Interpreter {

  def cata[S[_[_], _], F[_]](alg: HAlgebra[S, F])(implicit ev: HFunctor[S]) =
    new CataInterpreter[S, F](alg)

  def hylo[S[_[_], _], F[_], G[_]](coalg: HCoalgebra[S, G], alg: HAlgebra[S, F])(
    implicit ev: HFunctor[S]
  ) = new HyloInterpreter(coalg, alg)

}
