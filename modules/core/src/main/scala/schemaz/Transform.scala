package schemaz

trait Transform[F[_]] {
  def apply[A, B](fa: F[A], niso: NIso[A, B]): F[B]
}
