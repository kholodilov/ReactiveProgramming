package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import org.scalacheck.{Shrink, Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

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

  test("demux outputs are always false if in is false") {
    check {
      (demuxInOut: (Wire, List[Wire], List[Wire])) =>
        val (in, c, out) = demuxInOut
        demux(in, c, out)
        in.setSignal(false)

        run

        (out map { _.getSignal }) == (out map { _ => false })
    }
  }

  test("demux output are specified by control signal") {
    check {
      (demuxInOut: (Wire, List[Wire], List[Wire])) =>
        val (in, c, out) = demuxInOut
        demux(in, c, out)
        in.setSignal(true)

        val expOutN = controlToNum(c)
        val expOutSignals = booleanListWithOneTrueValue(expOutN, out.size).reverse

        run

        val outSignals = out map { _.getSignal }
        println(expOutN)
        println(outSignals)
        println(expOutSignals)

        outSignals == expOutSignals
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

  def booleanListWithOneTrueValue(pos: Int, size: Int): List[Boolean] = {
    List(
      (0 until pos) map {
        _ => false
      },
      List(true),
      ((pos + 1) until size) map {
        _ => false
      }).flatten
  }

  lazy val genWire: Gen[Wire] = for {
    s <- arbitrary[Boolean]
  } yield Wire(s)

  lazy val genWireList: Gen[List[Wire]] = Gen.sized { size => Gen.listOfN(size, genWire) }
  lazy val genDemuxInOut: Gen[(Wire, List[Wire], List[Wire])] = for {
    in <- genWire
    c <- genWireList
    out <- Gen.resize(Math.pow(2, c.size).toInt, genWireList)
  } yield (in, c, out)

  implicit lazy val arbDemuxInOut: Arbitrary[(Wire, List[Wire], List[Wire])] = Arbitrary(genDemuxInOut)
  override implicit val generatorDrivenConfig = PropertyCheckConfig(maxSize = 4)
  implicit val disableDemuxInOutShrink: Shrink[(Wire, List[Wire], List[Wire])] = Shrink(_ => Stream.empty)

}
