package nodescala

import scala.language.postfixOps
import scala.concurrent._
import ExecutionContext.Implicits.global
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
class FutureOpsSuite extends FunSuite with ShouldMatchers {

  // now

  test("now should return a value if future is completed") {
    val future = Future.always(123)

    future.now should equal (123)
  }

  test("now throws NoSuchElementException if future is not completed") {
    intercept[NoSuchElementException] {
      val future = Future.delay(1 second)
      future.now
    }
  }

  test("now throws future's exception if future is failed") {
    intercept[SomeException] {
      val future = Future.alwaysFails(new SomeException("now failure"))
      future.now
    }
  }

  // continueWith

  test("continueWith returns future of result returned by closure") {
    val future: Future[Int] = Future.always(123)

    val result: Future[String] = future continueWith { f => f.now.toString }

    Await.result(result, 1 second) should equal ("123")
  }

  test("continueWith returns future of result returned by closure even if original future failed") {
    val exception = new SomeException("closure fail")
    val future: Future[Int] = Future.alwaysFails(exception)

    val result: Future[String] = future continueWith { _ => "smth" }

    Await.result(result, 1 second) should equal ("smth")
  }

  test("continueWith returns future with failure if closure fails") {
    val exception = new SomeException("closure fail")
    val future: Future[Int] = Future.always(123)

    val result: Future[String] = future continueWith { f => throw exception }

    Await.result(result.failed, 1 second) should equal (exception)
  }

  // continue

  test("continue returns future of result returned by closure") {
    val future: Future[Int] = Future.always(123)

    val result: Future[String] = future continue { f => f.get.toString }

    Await.result(result, 1 second) should equal ("123")
  }

  test("continue returns future of result returned by closure even if original future failed") {
    val exception = new SomeException("closure fail")
    val future: Future[Int] = Future.alwaysFails(exception)

    val result: Future[String] = future continue { _ => "smth" }

    Await.result(result, 1 second) should equal ("smth")
  }

  test("continue returns future with failure if closure fails") {
    val exception = new SomeException("closure fail")
    val future: Future[Int] = Future.always(123)

    val result: Future[String] = future continue { f => throw exception }

    Await.result(result.failed, 1 second) should equal (exception)
  }

  // ensuring

  test("ensuring returns future with the result of current future if another future also succeeds") {
    val thisFuture = Future.always(123)
    val thatFuture = Future.always(456)

    val newFuture = thisFuture.ensuring(thatFuture)

    Await.result(newFuture, 1 second) should equal (123)
  }

  test("ensuring returns future with error if another future fails") {
    val exception = new SomeException("another future fails")
    val thisFuture = Future.always(123)
    val thatFuture = Future.alwaysFails(exception)

    val newFuture = thisFuture.ensuring(thatFuture)

    Await.result(newFuture.failed, 1 second) should equal (exception)
  }

  test("ensuring returns future with error if current future fails") {
    val exception = new SomeException("current future fails")
    val thisFuture = Future.alwaysFails(exception)
    val thatFuture = Future.always(456)

    val newFuture = thisFuture.ensuring(thatFuture)

    Await.result(newFuture.failed, 1 second) should equal (exception)
  }

  test("ensuring returns future with error of another future if both current and another future fail") {
    val exception1 = new SomeException("current future fails")
    val exception2 = new SomeException("another future fails")
    val thisFuture = Future.alwaysFails(exception1)
    val thatFuture = Future.alwaysFails(exception2)

    val newFuture = thisFuture.ensuring(thatFuture)

    Await.result(newFuture.failed, 1 second) should equal (exception2)
  }

}
