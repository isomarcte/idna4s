package org.typelevel.idna4s.tests.uts46

import cats._
import cats.data._
import cats.derived.semiauto
import cats.syntax.all._
import java.lang.StringBuilder
import org.typelevel.idna4s.core._
import scala.collection.immutable.SortedSet

trait UTS46ConformanceTestBase {
  protected def conformanceTestLines: Chain[String]

  final lazy val conformanceTestsFromFileContents: Ior[NonEmptyChain[Throwable], SortedSet[ConformanceTest]] =
    UTS46ConformanceTestBase.conformanceTestsFromLines(conformanceTestLines)
}

object UTS46ConformanceTestBase {

  def conformanceTestsFromLines[F[_]: Foldable](lines: F[String]): Ior[NonEmptyChain[Throwable], SortedSet[ConformanceTest]] =
    lines.parFoldMapA(line =>
      line.trim match {
        case line if line.startsWith("#") || line.isEmpty =>
          SortedSet.empty[ConformanceTest].rightIor[NonEmptyChain[Throwable]]
        case line =>
          Ior.fromEither(
            ConformanceTest.fromLine(line).bimap(
              NonEmptyChain.one,
              value => SortedSet(value)
            )
          )
      }
    )

  def conformanceTestsFromFileContents(value: String): Ior[NonEmptyChain[Throwable], SortedSet[ConformanceTest]] =
    conformanceTestsFromLines(value.linesIterator.toList)
}
