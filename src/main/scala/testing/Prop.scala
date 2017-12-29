package testing

trait Prop {

  type SuccessCount = Int

  type FailedCase = String

  def check: Either[FailedCase, SuccessCount]

  def &&(other: Prop): Prop = new Prop {
    override def check: Either[FailedCase, SuccessCount] = this.check match {
      case l@Left(s) => l
      case _@Right(c1) => {
        other.check match {
          case l@Left(s) => l
          case r@Right(c2) => Right(c1 + c2)
        }
      }
    }
  }
}
