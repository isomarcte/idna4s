package org.typelevel.idna4s.tests.uts46

import cats._
import cats.data._
import cats.derived.semiauto
import cats.syntax.all._
import java.lang.StringBuilder
import org.typelevel.idna4s.core._
import scala.collection.immutable.SortedSet

trait UTS46ConformanceTestBase {

  /** The conformance test lines from some source file, usually generated from a
    * file on unicode.org.
    *
    * Some filtering may take place during code generation so that each line
    * ''should'' be a valid [[ConformanceTest]]. The `Int` value is the line
    * number in the original source document so that test failures can be
    * easily mapped back to a line in the source document.
    */
  protected def conformanceTestLines: Chain[(String, Int)]

  final lazy val conformanceTestsFromFileContents: Ior[NonEmptyChain[(Throwable, Int)], SortedSet[(ConformanceTest, Int)]] =
    UTS46ConformanceTestBase.conformanceTestsFromLines(conformanceTestLines)

  final def unsafeConformanceTestsFromFileContents: SortedSet[(ConformanceTest, Int)] = {
    def prettyErrors(value: (Throwable, Int)): String =
      value match {
        case (error, lineNumber) =>
          s"${value}::${lineNumber}"
      }

    def errorMsg(value: NonEmptyChain[(Throwable, Int)]): String =
      s"Failed to parse ${value.size} conformance tests, ${value.map(prettyErrors)}"

    conformanceTestsFromFileContents.fold(
      l => throw new AssertionError(errorMsg(l)),
      r => r,
      (l, _) => throw new AssertionError(errorMsg(l))
    )
  }
}

object UTS46ConformanceTestBase {

  def conformanceTestsFromLines[F[_]: Foldable](lines: F[(String, Int)]): Ior[NonEmptyChain[(Throwable, Int)], SortedSet[(ConformanceTest, Int)]] =
    lines.parFoldMapA{
      case (line, lineNumber) =>
        line.trim match {
          case line if line.startsWith("#") || line.isEmpty =>
            SortedSet.empty[(ConformanceTest, Int)].rightIor[NonEmptyChain[(Throwable, Int)]]
          case line =>
            Ior.fromEither(
              ConformanceTest.fromLine(line).bimap(
                error => NonEmptyChain.one(error -> lineNumber),
                value => SortedSet(value -> lineNumber)
              )
            )
        }
    }
}
