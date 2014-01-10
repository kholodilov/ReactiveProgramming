package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite with Checkers {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val (in1, in2, out) = wiresWithProbes3
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

  test("invertor implements NOT") {
    check {
      (s: Boolean) =>
        val (in, out) = wiresWithProbes2
        inverter(in, out)
        in.setSignal(s)

        run

        out.getSignal == !s
    }
  }

  test("andGate implements AND") {
    check {
      (s1: Boolean, s2: Boolean) =>
        val (in1, in2, out) = wiresWithProbes3
        andGate(in1, in2, out)
        in1.setSignal(s1)
        in2.setSignal(s2)

        run

        out.getSignal == (s1 && s2)
    }
  }

  test("orGate implements OR") {
    check {
      (s1: Boolean, s2: Boolean) =>
        val (in1, in2, out) = wiresWithProbes3
        orGate(in1, in2, out)
        in1.setSignal(s1)
        in2.setSignal(s2)

        run

        out.getSignal == (s1 || s2)
    }
  }

  test("orGate2 implements OR") {
    check {
      (s1: Boolean, s2: Boolean) =>
        val (in1, in2, out) = wiresWithProbes3
        orGate2(in1, in2, out)
        in1.setSignal(s1)
        in2.setSignal(s2)

        run

        out.getSignal == (s1 || s2)
    }
  }

  def wiresWithProbes2: (Wire, Wire) = {
    val in, out = new Wire
    probe("in", in)
    probe("out", out)

    (in, out)
  }

  def wiresWithProbes3: (Wire, Wire, Wire) = {
    val in1, in2, out = new Wire
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)

    (in1, in2, out)
  }

}
