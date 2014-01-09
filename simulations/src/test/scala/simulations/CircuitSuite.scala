package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val (in1, in2, out) = wiresWithProbes
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //

  test("orGateTestFalseOrFalse") {
    val (in1, in2, out) = wiresWithProbes
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)

    run

    assert(out.getSignal === false, "false OR false")
  }

  test("orGateTestTrueOrFalse") {
    val (in1, in2, out) = wiresWithProbes
    orGate(in1, in2, out)
    in1.setSignal(true)
    in2.setSignal(false)

    run

    assert(out.getSignal === true, "true OR false")
  }

  test("orGateTestFalseOrTrue") {
    val (in1, in2, out) = wiresWithProbes
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(true)

    run

    assert(out.getSignal === true, "false OR true")
  }

  test("orGateTestTrueOrTrue") {
    val (in1, in2, out) = wiresWithProbes
    orGate(in1, in2, out)
    in1.setSignal(true)
    in2.setSignal(true)

    run

    assert(out.getSignal === true, "true OR true")
  }

  def wiresWithProbes: (Wire, Wire, Wire) = {
    val in1, in2, out = new Wire
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)

    (in1, in2, out)
  }

}
