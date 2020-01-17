package schemaz

trait Tagged[Repr]
final class Tag[Repr] {
  def apply[A](a: A): A with Tagged[Repr] = a.asInstanceOf[A with Tagged[Repr]]
}

object Tag {
  def apply[Repr] = new Tag[Repr]
}
