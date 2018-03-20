package catslib.casestudy.cedt

import cats._
import cats.implicits._

case class GCounterGen[A](counters: Map[String, A]) {

  def increment(machine: String, amount: A)(implicit ma: Monoid[A]) = {
    GCounterGen(counters + (machine -> (amount |+| counters.getOrElse(machine, ma.empty))))
  }
  def merge(that: GCounterGen[A])(implicit b: BoundedSemiLattice[A]): GCounterGen[A] = {
    GCounterGen(that.counters |+| counters)
  }
  def total(implicit m: Monoid[A]): A = counters.values.toList.combineAll
}
