package org.typelevel.idna4s.tests.uts46

import cats._
import cats.data._
import cats.syntax.all._
import munit.DisciplineSuite
import scala.collection.immutable.SortedMap

final class UTS46ConformanceTest extends DisciplineSuite {

  test("UTS-46 Conformance test") {
    GeneratedUTS46ConformanceTest.unsafeConformanceTestsFromFileContents.foreach{
      case value =>
        assert(clue(clue(value)._1.test).isEmpty)
    }
  }
}
