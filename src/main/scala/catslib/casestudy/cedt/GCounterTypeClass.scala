package catslib.casestudy.cedt

import cats._
import cats.implicits._
import catslib.casestudy.cedt._
import catslib.casestudy.cedt.KeyValueStoreSyntax._

trait GCounterTypeClass[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V]
  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]
  def total(f: F[K, V])(implicit m: Monoid[V]): V
}

object GCounterTypeClass {

  def apply[F[_, _], K, V](implicit counter: GCounterTypeClass[F, K, V]) = counter

  implicit def mapGCounter = new GCounterTypeClass[Map, String, Int] {
    override def increment(f: Map[String, Int])(k: String, v: Int)(implicit m: Monoid[Int]): Map[String, Int] = {
      f + (k -> (v |+| f.getOrElse(k, m.empty)))
    }
    override def merge(f1: Map[String, Int], f2: Map[String, Int])(implicit b: BoundedSemiLattice[Int]): Map[String, Int] = {
      f1 |+| f2
    }
    override def total(f: Map[String, Int])(implicit m: Monoid[Int]): Int = {
      f.values.toList.combineAll
    }
  }

  implicit def gcounterInstance[F[_, _], K, V](implicit kvs: KeyValueStore[F], km: Monoid[F[K, V]]) = new GCounterTypeClass[F, K, V] {
    override def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V] = {
      f.put(k, f.getOrElse(k, m.empty) |+| v)
    }
    override def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] = {
      f1 |+| f2
    }
    override def total(f: F[K, V])(implicit m: Monoid[V]): V = f.values.combineAll
  }
}