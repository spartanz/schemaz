package scalaz

package schema

package generic

trait ShowModule[R <: Realisation] extends GenericSchemaModule[R] {
  import Schema._

  implicit val showDecidableInstance: Decidable[Show] = new Decidable[Show] {
    override def choose2[Z, A1, A2](a1: => Show[A1], a2: => Show[A2])(f: Z => A1 \/ A2): Show[Z] =
      Show.shows[Z](z => f(z).fold(a1.shows, a2.shows))

    override def conquer[A]: Show[A] = Show.shows[A](_ => "")

    override def divide[A, B, C](fa: Show[A], fb: Show[B])(f: C => (A, B)): Show[C] = Show.shows[C](
      c => {
        val tpl = f(c)
        s"""(${fa.shows(tpl._1)}, ${fb.shows(tpl._2)})"""
      }
    )
  }

  private def prependLabel[L](showL: L => String): λ[X => (L, Show[X])] ~> Show =
    new (λ[X => (L, Show[X])] ~> Show) {
      override def apply[X](tpl: (L, Show[X])): Show[X] =
        Show.shows[X](x => s"""${showL(tpl._1)} = ${tpl._2.shows(x)}""")
    }

  implicit def algebra(
    primNT: R.Prim ~> Show,
    prodLabelToString: R.ProductTermId => String,
    sumLabelToString: R.SumTermId => String
  ): HAlgebra[Schema[R.Prim, R.SumTermId, R.ProductTermId, ?[_], ?], Show] =
    contravariantTargetFunctor(
      primNT,
      λ[Show ~> λ[X => Show[List[X]]]](
        show =>
          Show.shows(
            lst => lst.map(show.shows).mkString("[", ",", "]")
          )
      ),
      prependLabel(prodLabelToString),
      prependLabel(sumLabelToString),
      Show.shows[Unit](_ => "()")
    )
}
