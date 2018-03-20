package catslib.casestudy.cedt

case class GCounter(counters: Map[String, Int]) {

  def increment(machine: String, amount: Int) = {
    val v = amount + counters.getOrElse(machine, 0)
    GCounter(counters + (machine -> v))
  }
  def merge(that: GCounter): GCounter = {
    GCounter(that.counters ++ this.counters.map {
      case (k, v) => k -> (v max that.counters.getOrElse(k, 0))
    })
  }
  def total: Int = counters.values.sum
}
