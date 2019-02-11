package scalaz

package schema

package generic

import recursion._

trait ShowModule[R <: Realisation] extends GenericSchemaModule[R] {

  implicit val showDecidableInstance: Decidable[Show] = new Decidable[Show] {
    override def choose2[Z, A1, A2](a1: => Show[A1], a2: => Show[A2])(f: Z => A1 \/ A2): Show[Z] =
      Show.shows[Z](z => f(z).fold(a1.shows, a2.shows))

    override def conquer[A]: Show[A] = Show.shows[A](_ => "")

    override def divide[A, B, C](fa: Show[A], fb: Show[B])(f: C => (A, B)): Show[C] = Show.show[C](
      c => {
        val tpl = f(c)

        val lhs = fa.show(tpl._1)
        val rhs = fb.show(tpl._2)

        (lhs.isEmpty, rhs.isEmpty) match {
          case (true, true)  => Cord()
          case (true, false) => rhs
          case (false, true) => lhs
          case (false, false) =>
            Cord("(") ++ lhs ++ Cord(", ") ++ rhs ++ Cord(")")
        }
      }
    )
  }

  def showAlgebra(
    primNT: R.Prim ~> Show,
    prodLabelToString: R.ProductTermId => String,
    sumLabelToString: R.SumTermId => String
  ): HAlgebra[RSchema, Show] =
    contravariantTargetFunctor(
      primNT,
      λ[Show ~> λ[X => Show[List[X]]]](
        show =>
          Show.shows(
            lst => lst.map(show.shows).mkString("[", ",", "]")
          )
      ),
      λ[RProductTerm[Show, ?] ~> Show](
        showL => Show.shows(x => s"""${prodLabelToString(showL.id)} = (${showL.schema.shows(x)})""")
      ),
      λ[RSumTerm[Show, ?] ~> Show](
        showL => Show.shows(x => s"""${sumLabelToString(showL.id)} = (${showL.schema.shows(x)})""")
      ),
      Show.shows[Unit](_ => "()")
    )
}
