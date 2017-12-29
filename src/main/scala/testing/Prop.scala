package testing

trait Prop {

  def check: Boolean

  def &&(other: Prop): Prop = new Prop {
    override def check: Boolean = this.check && other.check
  }
}
