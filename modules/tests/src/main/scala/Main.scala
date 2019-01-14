package scalaz

package schema

package tests

import testz._
import testz.runner._

import scala.Console._
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration.Duration

object Main {

  def printReport(scope: List[String], out: Result): List[String] =
    "\n" :: (out match {
      case Succeed =>
        Runner.fastConcatDelim(new ::(s"[${GREEN}OK${RESET}]\n", scope), " > ")
      case fail: Fail =>
        Runner.fastConcatDelim(new ::(s"[${RED}KO${RESET}]\n", scope), " > ") ++ fail.failures
          .map {
            case Left(t)    => t.getMessage
            case Right(err) => err
          }
    })

  val harness: Harness[PureHarness.Uses[Unit]] =
    PureHarness.toHarness(
      PureHarness.make(
        (ls, tr) => Runner.printStrs(printReport(ls, tr), print)
      )
    )

  def suites(harness: Harness[PureHarness.Uses[Unit]]): List[() => Future[TestOutput]] =
    tests(harness).map {
      case (name, suite) =>
        () => Future.successful(suite((), List(name)))
    }

  def tests[T](harness: Harness[T]): List[(String, T)] =
    List(
      ("Examples", SchemaModuleExamples.tests(harness)),
      ("JSON", JsonExamples.tests(harness)),
      ("Scalacheck generators", GenModuleExamples.tests(harness))
    )

  def main(args: Array[String]): Unit = {
    val result = Await.result(Runner(suites(harness), global), Duration.Inf)

    if (result.failed) throw new Exception("some tests failed")
  }
}
