package scalaz

package schema

package play.json

import scalaz.schema.TestMain
import testz.Harness
import testz.runner.Runner

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration.Duration

object Main extends TestMain {

  def tests[T](harness: Harness[T]): List[(String, T)] =
    List(
      ("Examples", PlayJsonExamples.tests(harness))
    )

  def main(args: Array[String]): Unit = {
    val result = Await.result(Runner(suites(harness), global), Duration.Inf)

    if (result.failed) throw new Exception("some tests failed")
  }
}
