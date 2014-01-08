package testpackage

object TestWorksheet {
	case class Calculator(brand: String, model: String)
	val hp20b = Calculator("hp", "20b")       //> hp20b  : testpackage.TestWorksheet.Calculator = Calculator(hp,20b)
	val hp20b2 = Calculator("hp", "20b")      //> hp20b2  : testpackage.TestWorksheet.Calculator = Calculator(hp,20b)
	val hp30b = Calculator("hp", "30b")       //> hp30b  : testpackage.TestWorksheet.Calculator = Calculator(hp,30b)
	hp20b == hp20b2                           //> res0: Boolean = true
	hp20b == hp30b                            //> res1: Boolean = false
	
	def calcType(calc: Calculator): String = calc match {
	    case Calculator("hp", "20b") => "financial"
	    case Calculator("hp", "30b") => "business"
	    case Calculator("hp", "48g") => "scientific"
	    case Calculator(_, _)        => "unknown"
	}                                         //> calcType: (calc: testpackage.TestWorksheet.Calculator)String
	calcType(hp20b)                           //> res2: String = financial
	calcType(hp30b)                           //> res3: String = business
}