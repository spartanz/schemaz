package scalaz

package schema

trait JsonModule extends SchemaModule {
  type JSON = String

  type Encoder[A] = A => JSON

  implicit def representation(implicit primsNt: Prim ~> Encoder) =
    new Schema.Representation[Encoder] {

      val prims = primsNt

      override val handleRecord = new (Encoder ~> Encoder) {
        def apply[A](fa: Encoder[A]): Encoder[A] = fa.andThen("{" + _ + "}")
      }

      override val handleUnion = handleRecord

      override val labelField =
        (label: ProductTermId) =>
          new (Encoder ~> Encoder) {
            def apply[A](fa: Encoder[A]): Encoder[A] = fa.andThen(s""""$label":""" + _)
          }

      override val labelBranch =
        (label: SumTermId) =>
          new (Encoder ~> Encoder) {
            def apply[A](fa: Encoder[A]): Encoder[A] = fa.andThen(s""""$label":""" + _)
          }

      def handleList[A]: Encoder[A] => Encoder[List[A]] = {
        encA =>
          { list =>
            list.map(encA).mkString("[", ",", "]")

          }

      }

      def unit: Encoder[Unit] = (_ => "null")

    }

  def functionDecidable[X: Monoid]: Decidable[? => X] = new Decidable[? => X] {
    override def choose2[Z, A1, A2](a1: => (A1 => X), a2: => (A2 => X))(
      f: Z => A1 \/ A2
    ): (Z => X) = z => f(z).fold(a1, a2)

    override def conquer[A]: A => X = _ => Monoid[X].zero

    override def divide[A, B, C](fa: A => X, fb: B => X)(f: C => (A, B)): C => X = c => {
      val tpl = f(c)
      Monoid[X].append(fa(tpl._1), fb(tpl._2))
    }

    override def contramap[A, B](fa: A => X)(f: B => A): B => X = fa.compose(f)
  }

  implicit val encoderDecidable = functionDecidable(new Monoid[String] {
    def zero                            = ""
    def append(a: String, b: => String) = s"$a, $b"
  })
}
