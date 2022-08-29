package org.typelevel.idna4s.build

import scala.annotation.tailrec
import scala.util.matching._
import scala.util.Try
import scala.io.Source
import java.io.File
import sbt._

// object UTS46IDNAMappingTablePlugin extends AutoPlugin {

//   object autoImport {
//     val UTS46MappingUnicodeVersion = settingKey[String]("The Unicode version to use for generating the UTS-46 IDNA mapping tables.")
//     val generateUTS46IDNAMapping = taskKey[Unit]("Generate the code for the UTS-46 IDNA mapping tables.")
//   }

//   override def trigger = noTrigger
// }

object UTS46IDNAMappingTable {
  sealed abstract class CodePoint extends Serializable {
    def value: Int

    override final def toString: String = s"CodePoint(value = ${value})"
  }

  object CodePoint {
    private[this] final case class CodePointImpl(override val value: Int) extends CodePoint

    def fromInt(value: Int): Either[String, CodePoint] =
      if (value < 0 || value > Character.MAX_CODE_POINT) {
        Left(s"Invalid code point: ${value}")
      } else {
        Right(CodePointImpl(value))
      }
  }

  final case class UnicodeVersion(value: String) extends AnyVal

  sealed abstract class InputCodePoints extends Serializable

  object InputCodePoints {
    val rangeRegex: Regex =
      """([0-9A-Fa-f]{1,6})\.\.([0-9A-Fa-f]{1,6})""".r
    val singleRegex: Regex =
      """([0-9A-Fa-f]{1,6})""".r

    def fromString(value: String): Either[String, InputCodePoints] =
      value.trim match {
        case rangeRegex(lower, upper) =>
          for {
            lower <- CodePoint.fromInt(Integer.parseInt(lower, 16))
            upper <- CodePoint.fromInt(Integer.parseInt(upper, 16))
            result <- CodePointRange.from(lower, upper)
          } yield result
        case singleRegex(value) =>
          CodePoint.fromInt(Integer.parseInt(value, 16)).map(SingleCodePoint.apply _)
        case _ =>
          Left(s"Unable to parse code points: ${value}.")
      }

    final case class SingleCodePoint(value: CodePoint) extends InputCodePoints
    sealed abstract class CodePointRange extends InputCodePoints {
      def lower: CodePoint
      def upper: CodePoint

      override final def toString: String = s"CodePointRange(lower = $lower, upper = $upper)"
    }

    object CodePointRange {
      private[this] final case class CodePointRangeImpl(override val lower: CodePoint, override val upper: CodePoint) extends CodePointRange

      def from(lower: CodePoint, upper: CodePoint): Either[String, CodePointRange] =
        if (lower.value >= upper.value) {
          Left(s"Invalid bounds for code point range. Lower must be < upper, but got ${lower}, ${upper}")
        } else {
          Right(CodePointRangeImpl(lower, upper))
        }
    }
  }

  sealed abstract class IDNA2008Status extends Serializable

  object IDNA2008Status {
    case object NV8 extends IDNA2008Status
    case object XV8 extends IDNA2008Status

    def fromString(value: String): Either[String, IDNA2008Status] =
      value.trim match {
        case "NV8" => Right(NV8)
        case "XV8" => Right(XV8)
        case _ => Left(s"Unknown IDNA 2008 status string: ${value}")
      }
  }

  sealed abstract class CodePointStatus extends Serializable {
    import CodePointStatus._

    final def canHaveMapping: Boolean =
      this match {
        case Ignored => true
        case Mapped => true
        case Deviation => true
        case Disallowed_STD3_Mapped => true
        case _ => false
      }

    final def validateCanHaveMapping(mapping: String): Either[String, CodePointStatus] =
      if (canHaveMapping) {
        Right(this)
      } else {
        Left(s"Code point status ${this} can not have a mapping, but it does ($mapping).")
      }
  }

  object CodePointStatus {
    case object Valid extends CodePointStatus
    case object Ignored extends CodePointStatus
    case object Mapped extends CodePointStatus
    case object Deviation extends CodePointStatus
    case object Disallowed extends CodePointStatus
    case object Disallowed_STD3_Valid extends CodePointStatus
    case object Disallowed_STD3_Mapped extends CodePointStatus

    def fromString(value: String): Either[String, CodePointStatus] =
      value.trim match {
        case "valid" => Right(Valid)
        case "ignored" => Right(Ignored)
        case "mapped" => Right(Mapped)
        case "deviation" => Right(Deviation)
        case "disallowed" => Right(Disallowed)
        case "disallowed_STD3_valid" => Right(Disallowed_STD3_Valid)
        case "disallowed_STD3_mapped" => Right(Disallowed_STD3_Mapped)
        case _ => Left(s"Unknown code point status: ${value}")
      }
  }

  final case class MappingValue(value: List[CodePoint])

  object MappingValue {
    def fromString(value: String): Either[String, MappingValue] = {
      value.trim match {
        case value if value.isEmpty =>
          // Special case for when there is an IDNA 2008 status, but no
          // mapping value.
          Right(empty): Either[String, MappingValue]
        case value =>
          value.split(" ").toList.foldLeft(Right(List.empty[CodePoint]): Either[String, List[CodePoint]]){
            case (acc, value) =>
              acc.flatMap(acc =>
                CodePoint.fromInt(Integer.parseInt(value, 16)).map(_ :: acc)
              )
          }.map(_.reverse).map(MappingValue.apply)
      }
    }

    val empty: MappingValue = MappingValue(Nil)
  }

  final case class Comment(value: String)

  final case class Row(
    inputCodePoints: InputCodePoints,
    codePointStatus: CodePointStatus,
    mappingValue: MappingValue,
    idna2008Status: Option[IDNA2008Status],
    comment: Comment
  )

  object Row {
    def fromString(value: String): Either[String, Row] =
      value.split("""[;#]""").toList match {
        case input :: status :: comment :: Nil =>
          for {
            input <- InputCodePoints.fromString(input)
            status <- CodePointStatus.fromString(status)
          } yield Row(input, status, MappingValue.empty, None, Comment(comment))
        case input :: status :: mapping :: comment :: Nil =>
          for {
            input <- InputCodePoints.fromString(input)
            status <- CodePointStatus.fromString(status).flatMap(_.validateCanHaveMapping(mapping))
            mapping <- MappingValue.fromString(mapping)
          } yield Row(input, status, mapping, None, Comment(comment))
        case input :: status :: mapping :: idna2008Status :: comment :: Nil if mapping.trim.isEmpty =>
          // We can ignore mapping, because only Valid will have an IDNA2008Status and Valid can not have a mapping.
          for {
            input <- InputCodePoints.fromString(input)
            status <- CodePointStatus.fromString(status)
            idna2008Status <- IDNA2008Status.fromString(idna2008Status)
          } yield Row(input, status, MappingValue.empty, Some(idna2008Status), Comment(comment))
        case _ =>
          Left(s"Unable to parse row: ${value}")
      }
  }

  final case class Rows(
    version: UnicodeVersion,
    rows: List[Row]
  )

  object Rows {
    val unicodeVersionRegex: Regex =
      """#\s*Version:\s*(\d+\.\d+\.\d+)""".r

    def fromLines(lines: List[String]): Either[String, Rows] = {

      @tailrec
      def parseVersion(lines: List[String]): Either[String, (UnicodeVersion, List[String])] =
        lines match {
          case Nil =>
            Left("End of input reached without finding version string.")
          case x :: xs =>
            x match {
              case unicodeVersionRegex(version) => Right((UnicodeVersion(version), xs))
              case _ => parseVersion(xs)
            }
        }

      parseVersion(lines) match {
        case Left(error) => Left(error)
        case Right((version, rest)) =>
          rest.dropWhile(_.startsWith("#")).foldLeft(Right(List.empty[Row]): Either[String, List[Row]]){
            case (acc, value) if value.trim.isEmpty || value.trim.startsWith("#") =>
              acc
            case (acc, value) =>
              acc.flatMap(acc =>
                Row.fromString(value).map(_ :: acc)
              )
          }.map(_.reverse).map(rows => Rows(version, rows))
      }
    }

    def fromString(value: String): Either[String, Rows] =
      fromLines(value.linesIterator.toList)

    def fromFile(path: String): Either[String, Rows] =
      Try(new File(path)).flatMap(f =>
        Try(Source.fromFile(f)).flatMap{s =>
          val lines: List[String] = s.getLines.toList
          Try(s.close).map(_ => lines)
        }
      ).toEither.swap.map(_.getLocalizedMessage).swap.flatMap(fromLines)
  }
}
