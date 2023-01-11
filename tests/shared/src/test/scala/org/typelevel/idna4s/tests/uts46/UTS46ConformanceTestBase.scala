package org.typelevel.idna4s.tests.uts46

import cats._
import cats.data._
import cats.derived.semiauto
import cats.syntax.all._
import java.lang.StringBuilder
import org.typelevel.idna4s.core._
import scala.collection.immutable.SortedSet

trait UTS46ConformanceTestBase {
  protected def conformanceTestFileContents: String

  final def conformanceTestsFromFileContents: Ior[NonEmptyChain[String], SortedSet[ConformanceTest]] =
    UTS46ConformanceTestBase.conformanceTestsFromFileContents(conformanceTestFileContents)
}

object UTS46ConformanceTestBase {

  def conformanceTestsFromLines[F[_]: Foldable](lines: F[String]): Ior[NonEmptyChain[String], SortedSet[ConformanceTest]] =
    lines.parFoldMapA(line =>
      line.trim match {
        case line if line.startsWith("#") || line.isEmpty =>
          SortedSet.empty[ConformanceTest].rightIor[NonEmptyChain[String]]
        case line =>
          Ior.fromEither(
            ConformanceTest.fromLine(line).bimap(
              NonEmptyChain.one,
              value => SortedSet(value)
            )
          )
      }
    )

  def conformanceTestsFromFileContents(value: String): Ior[NonEmptyChain[String], SortedSet[ConformanceTest]] =
    conformanceTestsFromLines(value.linesIterator.toList)
}
