package org.typelevel.idna4s.tests.uts46

import cats._
import cats.data._
import cats.syntax.all._
import munit.DisciplineSuite
import scala.collection.immutable.SortedMap

final class UTS46ConformanceTest extends DisciplineSuite {
  GeneratedUTS46ConformanceTest.unsafeConformanceTestsFromFileContents.foreach{
    case value@(_, lineNumber) =>
      test(s"ConformanceTest from line number: ${lineNumber}") {
        assert(clue(clue(value)._1.test).isEmpty)
      }
  }
}
