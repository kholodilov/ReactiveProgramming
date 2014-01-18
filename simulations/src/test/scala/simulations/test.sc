import simulations.Wire

object testWorksheet {
  def controlToNum(c: List[Wire]): Int = {
    def wireSignalToNum(w: Wire): Int = {
      if (w.getSignal) 1 else 0
    }
    c.reverse match {
      case Nil => 0
      case w :: tail =>
        wireSignalToNum(w) + 2 * controlToNum(tail.reverse)
    }
  }

  val c = List(Wire(true), Wire(false), Wire(true))
  val expOutN = controlToNum(c)
  List((1 until expOutN) map { _ => false }, List(true), ((expOutN + 1) until (8 + 1)) map { _ => false }).flatten.reverse

}