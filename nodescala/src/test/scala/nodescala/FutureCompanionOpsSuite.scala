package nodescala

import scala.language.postfixOps
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.concurrent._
import scala.concurrent.duration._
import nodescala.util.SomeException

/**
 *
 * @author kholodilov
 */
@RunWith(classOf[JUnitRunner])
class FutureCompanionOpsSuite extends FunSuite with ShouldMatchers {

  test("A Future should always be created") {
    val always = Future.always(517)

    always.now should equal (517)
  }

  test("A Future should always throw exception") {
    val exception = new SomeException("always")
    val always = Future.alwaysFails(exception)

    always.failed.now should equal (exception)
  }

  test("A Future should never be created") {
    intercept[TimeoutException] {
      val never = Future.never[Int]

      Await.ready(never, 1 second)
    }
  }

  test("delay returns a future that is completed after specified time") {
    val delayFuture = Future.delay(1900 millis)

    try {
      Await.ready(delayFuture, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }

    Await.ready(delayFuture, 1 second)
  }

  // all

  test("list of futures converted to future of list") {
    val listOfFutures = List(Future.always(123), Future.always(456), Future.always(789))

    val futureOfList = Future.all(listOfFutures)

    Await.result(futureOfList, 1 second) should equal (List(123, 456, 789))
  }

  test("list of futures converted failed future if there are failure in some future") {
    val exception = new SomeException("some future fails")
    val listOfFutures = List(Future.always(123), Future.alwaysFails(exception), Future.always(789))

    val futureOfList = Future.all(listOfFutures)

    Await.result(futureOfList.failed, 1 second) should equal (exception)
  }

  test("empty list of futures converted to future of empty list") {
    val listOfFutures: List[Future[Int]] = List()

    val futureOfList = Future.all(listOfFutures)

    Await.result(futureOfList, 1 second) should equal (List())
  }

  // any

  test("any of list of one future returns this future") {
    val listOfFutures = List(Future.always(123))

    val result = Future.any(listOfFutures)

    Await.result(result, 1 second) should equal (123)
  }

  test("any of list of future returns future that completes first") {
    val future1 = Future.always(123) ensuring Future.delay(500 millis)
    val future2 = Future.always(456) ensuring Future.delay(200 millis) // this should comlete first
    val future3 = Future.always(789) ensuring Future.delay(300 millis)
    val listOfFutures  = List(future1, future2, future3)

    val result = Future.any(listOfFutures)

    Await.result(result, 1 second) should equal (456)
  }

  test("any of list of future returns failed future if it is first and fails") {
    val exception = new SomeException("some future fails")
    val future1 = Future.always(123) ensuring Future.delay(500 millis)
    val future2 = Future.alwaysFails(exception) ensuring Future.delay(200 millis) // this should comlete first
    val future3 = Future.always(789) ensuring Future.delay(300 millis)
    val listOfFutures  = List(future1, future2, future3)

    val result = Future.any(listOfFutures)

    Await.result(result.failed, 1 second) should equal (exception)
  }

  test("any of empty list returns future that never compeltes") {
    intercept[TimeoutException] {
      val listOfFutures = List()

      val result = Future.any(listOfFutures)

      Await.ready(result, 1 second)
    }
  }

}
