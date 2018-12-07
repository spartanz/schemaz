package scalaz

package schema

trait Thing[F[_]] {
  def choose[A, B](fa: F[A], fb: F[B]): F[Either[A, B]]
}

object Thing {
  def apply[F[_]](implicit instance: Thing[F]) = instance

  implicit def eitherFunction[T]: Thing[? => T] = new Thing[? => T] {
    def choose[A, B](fa: A => T, fb: B => T): Either[A, B] => T = e => e.fold(fa, fb)
  }
}
