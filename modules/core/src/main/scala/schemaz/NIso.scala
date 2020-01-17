package schemaz

import scalaz.\/

trait Distributes[P[_, _], Q[_, _]] {
  def dist[A0, A1, B0, B1](pa: P[A0, A1], pb: P[B0, B1]): P[Q[A0, B0], Q[A1, B1]]
}

final case class NIso[A, B](f: A => B, g: B => A) {

  def compose[C](other: NIso[B, C]): NIso[A, C] = NIso(other.f.compose(f), g.compose(other.g))
}

object NIso {

  def id[A] = NIso[A, A](identity, identity)

  implicit val nisoDistributesOverProduct: Distributes[NIso, Tuple2] =
    new Distributes[NIso, Tuple2] {

      def dist[A0, A1, B0, B1](pa: NIso[A0, A1], pb: NIso[B0, B1]): NIso[(A0, B0), (A1, B1)] =
        NIso(p0 => (pa.f(p0._1), pb.f(p0._2)), p1 => (pa.g(p1._1), pb.g(p1._2)))
    }

  implicit val nisoDistributesOverSum: Distributes[NIso, \/] = new Distributes[NIso, \/] {

    def dist[A0, A1, B0, B1](pa: NIso[A0, A1], pb: NIso[B0, B1]): NIso[A0 \/ B0, A1 \/ B1] =
      NIso(e0 => e0.bimap(pa.f, pb.f), e1 => e1.bimap(pa.g, pb.g))
  }

}
