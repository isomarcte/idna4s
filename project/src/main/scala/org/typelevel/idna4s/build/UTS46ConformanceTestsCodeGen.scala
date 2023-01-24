package org.typelevel.idna4s.build

import cats._
import cats.data._
import cats.syntax.all._
import java.net.URI
import java.net.URL
import sbt.{Show => _, _}
import scala.meta._
import java.nio.charset.StandardCharsets

object UTS46ConformanceTestsCodeGen {

  final private val BaseName = "UTS46ConformanceTest"
  final private val BaseTypeName: String = s"${BaseName}Base"
  final private val GeneratedTypeName: String = s"Generated${BaseName}"

  def unsafeGenerate(
    dir: File,
    unicodeVersion: String
  ): Seq[File] =
    generate(dir, UnicodeVersion.unsafeFromString(unicodeVersion)).fold(
      e => throw e,
      file => List(file)
    )

  def generate(
    dir: File,
    unicodeVersion: UnicodeVersion
  ): Either[Throwable, File] = {
    val outputFile: File =
      dir / "org" / "typelevel" / "idna4s" / "tests" / "uts46" / s"${GeneratedTypeName}.scala"

    conformanceTestFileContentsFromUnicodeVersion(unicodeVersion).map(_.zipWithIndex.map{
      case (line, lineNumber) => line -> (lineNumber + 1)
    }).map(lines =>
      lines.filterNot{
        case (line, _) =>
          line.trim match {
            case line =>
              line.isEmpty || line.startsWith("#")
          }
      }
    ).map(lines =>
      lines.map{
        case (line, index) => q"($line, $index)"
      }
    ).map(generateSource).flatMap(tree =>
      Either.catchNonFatal(IO.write(outputFile, tree.syntax)).as(outputFile)
    )
  }

  def conformanceTestFileContentsFromUnicodeVersion(unicodeVersion: UnicodeVersion): Either[Throwable, List[String]] =
    Either.catchNonFatal(
      URI.create(s"https://www.unicode.org/Public/idna/${unicodeVersion.asString}/IdnaTestV2.txt").toURL()
    ).flatMap(url =>
      Either.catchNonFatal(
        IO.readLinesURL(url, StandardCharsets.UTF_8)
      )
    )

  def generateSource(
    lines: List[Term]
  ): Tree = {
    // Workaround method too large
    val (part0, part1): (List[Term], List[Term]) =
      lines.splitAt(lines.size/2)

    source"""
package org.typelevel.idna4s.tests.uts46

import cats.data._

object ${Term.Name(GeneratedTypeName)} extends ${Init(Type.Name(BaseTypeName), scala.meta.Name(""), Seq.empty)} {

  private[this] final def conformanceTestLines0: Chain[(String, Int)] = Chain(..$part0)
  private[this] final def conformanceTestLines1: Chain[(String, Int)] = Chain(..$part1)
  override final lazy val conformanceTestLines: Chain[(String, Int)] = conformanceTestLines0 ++ conformanceTestLines1
}
"""
  }
}
