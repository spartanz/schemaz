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

        val lhs = fa.show(tpl._1)
        val rhs = fb.show(tpl._2)

        (lhs, rhs) match {
          case (s1, s2) if s1.isEmpty && s2.isEmpty  => ""
          case (s1, s2) if s1.isEmpty && !s2.isEmpty => Show[Cord].shows(s2)
          case (s1, s2) if !s1.isEmpty && s2.isEmpty => Show[Cord].shows(s1)
          case (s1, s2) if !s1.isEmpty && !s2.isEmpty =>
            s"""(${Show[Cord].shows(s1)}, ${Show[Cord].shows(s2)})"""
        }
      }
    )
  }

  def showAlgebra(
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
      λ[RProductTerm[Show, ?] ~> Show](
        showL => Show.shows(x => s"""${prodLabelToString(showL.id)} = (${showL.schema.shows(x)})""")
      ),
      λ[RSumTerm[Show, ?] ~> Show](
        showL => Show.shows(x => s"""${sumLabelToString(showL.id)} = (${showL.schema.shows(x)})""")
      ),
      Show.shows[Unit](_ => "()")
    )
}
