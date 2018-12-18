package scalaz

package schema

import testz._
import testz.runner._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration.Duration

object Main extends TestMain {

  def tests[T](harness: Harness[T]): List[(String, T)] =
    List(
      //("Examples", SchemaModuleExamples.tests(harness)),
      ("JSON", JsonExamples.tests(harness))
    )

  def main(args: Array[String]): Unit = {
    val result = Await.result(Runner(suites(harness), global), Duration.Inf)

    if (result.failed) throw new Exception("some tests failed")
  }
}
