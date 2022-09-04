package org.typelevel.idna4s.build

import cats._
import cats.data._
import cats.syntax.all._
import java.io.File
import java.net.URL
import sbt._
import scala.annotation.tailrec
import scala.io.Source
import scala.meta._
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.matching._
import scala.collection.immutable.SortedSet
import scala.collection.immutable.SortedMap

/** Functions for generating the UTS-46 lookup tables for step 1 of UTS-46
  * processing.
  *
  * @see [[https://www.unicode.org/reports/tr46/#Processing UTS-46 Section 4]]
  * @see [[https://www.unicode.org/reports/tr46/#IDNA_Mapping_Table Section 5]]
  */
object UTS46IDNAMappingTable {
  import LookupTableGen._

  // Developer's note: One will note a lot of code devoted to supporting
  // Order/Ordering instances. We want to ensure that the generated tables are
  // in order both for readability and to increase the likelihood we generate
  // a tableswitch

  // Sentinel values used for signaling non-mapped code points, or single code
  // points which are mapped to multi output code points.
  val VALID: Int = -1
  val VALID_NV8: Int = -2
  val VALID_XV8: Int = -3
  val IGNORED: Int = -4
  val MAPPED_MULTI_CODE_POINT = -5
  val DEVIATION = -6
  val DEVIATION_MULTI = -7
  val DISALLOWED = -8
  val DISALLOWED_STD3_VALID = -9
  val DISALLOWED_STD3_MAPPED = -10
  val DISALLOWED_STD3_MAPPED_MULTI = -11
  val DEVIATION_IGNORED = -12

  /** Newtype for a Unicode code point. */
  sealed abstract class CodePoint extends Serializable {
    def value: Int

    override final def toString: String = s"CodePoint(value = ${value})"
  }

  object CodePoint {
    private[this] final case class CodePointImpl(override val value: Int) extends CodePoint

    def unsafeFromInt(value: Int): CodePoint =
      fromInt(value).fold(
        e => throw new IllegalArgumentException(e),
        identity
      )

    def fromInt(value: Int): Either[String, CodePoint] =
      if (value < 0 || value > Character.MAX_CODE_POINT) {
        Left(s"Invalid code point: ${value}")
      } else {
        Right(CodePointImpl(value))
      }

    implicit val orderInstance: Order[CodePoint] =
      Order.by(_.value)
  }

  /** Newtype for a Unicode version string. */
  final case class UnicodeVersion(value: String) extends AnyVal

  /** ADT for the IDNA 2008 status associated with some UTS-46 valid code
    * points.
    */
  sealed abstract class IDNA2008Status extends Serializable {
    import IDNA2008Status._

    final def asString: String =
      this match {
        case NV8 => "NV8"
        case XV8 => "XV8"
      }
  }

  object IDNA2008Status {
    /** Valid under UTS-46, but excluded from all domain names under IDNA 2008 for
      * all versions of Unicode.
      */
    case object NV8 extends IDNA2008Status

    /** Valid under UTS-46, but excluded from all domain names under IDNA 2008 for
      * the ''current'' version of Unicode.
      */
    case object XV8 extends IDNA2008Status

    implicit val orderInstance: Order[IDNA2008Status] =
      new Order[IDNA2008Status] {
        override def compare(x: IDNA2008Status, y: IDNA2008Status): Int =
          (x, y) match {
            case (NV8, XV8) =>
              -1
            case (XV8, NV8) =>
              1
            case _ =>
              0
          }
      }

    implicit def orderingInstance: Ordering[IDNA2008Status] =
      orderInstance.toOrdering

    def fromString(value: String): Either[String, IDNA2008Status] =
      value.trim match {
        case "NV8" => Right(NV8)
        case "XV8" => Right(XV8)
        case _ => Left(s"Unknown IDNA 2008 status string: ${value}")
      }
  }

  /** ADT for parsing the status of a code point, with it's associated data.
    *
    * This represents a given row in the unparsed lookup table, without the
    * input code points and the comment.
    */
  sealed abstract class CodePointStatus extends Serializable {

    /** Generate the case body for the main mapping function. For example, in the
      * generated match "case i => VALID", this would be the string "VALID".
      */
    def asCaseBody: String
  }

  object CodePointStatus {
    implicit val orderInstance: Order[CodePointStatus] =
      new Order[CodePointStatus] {
        override def compare(x: CodePointStatus, y: CodePointStatus): Int =
          (x, y) match {
            case (Valid(x), Valid(y)) =>
              x.compare(y)
            case (Ignored, Ignored) =>
              0
            case (Mapped(x), Mapped(y)) =>
              x.compare(y)
            case (Deviation(x), Deviation(y)) =>
              x.compare(y)
            case (Disallowed, Disallowed) =>
              0
            case (Disallowed_STD3_Valid, Disallowed_STD3_Valid) =>
              0
            case (Disallowed_STD3_Mapped(x), Disallowed_STD3_Mapped(y)) =>
              x.compare(y)
            case (_: Valid, _) =>
              -1
            case (_, _: Valid) =>
              1
            case (Ignored, _) =>
              -1
            case (_, Ignored) =>
              1
            case (_: Mapped, _) =>
              -1
            case (_, _: Mapped) =>
              1
            case (_: Deviation, _) =>
              -1
            case (_, _: Deviation) =>
              1
            case (Disallowed, _) =>
              -1
            case (_, Disallowed) =>
              1
            case (Disallowed_STD3_Valid, _) =>
              -1
            case (_, Disallowed_STD3_Valid) =>
              1
          }

        implicit def orderingInstance: Ordering[CodePointStatus] =
          orderInstance.toOrdering
      }

    /** A valid code point, e.g. it maps to itself under UTS-46.
      *
      * Some code points which are valid under
      */
    final case class Valid(idna2008Status: Option[IDNA2008Status]) extends CodePointStatus {
      override final def asCaseBody: String = {
        idna2008Status.fold(
          "VALID"
        ){
          case IDNA2008Status.NV8 =>
            "VALID_NV8"
          case IDNA2008Status.XV8 =>
            "VALID_XV8"
        }
      }
    }

    /** A code point which is ignored under UTS-46, this means it is dropped from
      * the input string.
      */
    case object Ignored extends CodePointStatus {
      override final val asCaseBody: String =
        "IGNORED"
    }

    /** A code point which is mapped to one or more other code points. */
    final case class Mapped(mapping: NonEmptyList[CodePoint]) extends CodePointStatus {
      override final def asCaseBody: String =
        if (mapping.size === 1) {
          printAsHex(mapping.head.value)
        } else {
          "MAPPED_MULTI_CODE_POINT"
        }
    }

    /** A code point which represents a "deviation". Deviation code points can,
      * under certain circumstances, yield different IDNs under IDNA 2003
      * vs. IDNA 2008.
      *
      * They can map to zero or more code points.
      *
      * @see [[https://www.unicode.org/reports/tr46/#Deviations Deviations]]
      */
    final case class Deviation(mapping: List[CodePoint]) extends CodePointStatus {
      override final def asCaseBody: String =
        if (mapping.isEmpty) {
          "DEVIATION_IGNORED"
        } else if (mapping.size < 2) {
          "DEVIATION"
        } else {
          "DEVIATION_MULTI"
        }
    }

    /** Code points which are disallowed under UTS-46. Attempting to map a string
      * containing a disallowed code point will yield an error.
      */
    case object Disallowed extends CodePointStatus {
      override final def asCaseBody: String =
        "DISALLOWED"
    }

    /** Code points which are disallowed unless the UseSTD3ASCIIRules
      * configuration option is false (not recommended). When
      * UseSTD3ASCIIRules is false, these code points are valid, e.g. they map
      * to themselves.
      *
      * @see [[https://www.unicode.org/reports/tr46/#UseSTD3ASCIIRules UseSTD3ASCIIRules]]
      */
    case object Disallowed_STD3_Valid extends CodePointStatus {
      override final def asCaseBody: String =
        "DISALLOWED_STD3_VALID"
    }

    /** Code points which are disallowed unless the UseSTD3ASCIIRules
      * configuration option is false (not recommended). When
      * UseSTD3ASCIIRules is false, these code points are mapped to 1 or more
      * code points.
      *
      * @see [[https://www.unicode.org/reports/tr46/#UseSTD3ASCIIRules UseSTD3ASCIIRules]]
      */
    final case class Disallowed_STD3_Mapped(mapping: NonEmptyList[CodePoint]) extends CodePointStatus {
      override final def asCaseBody: String =
        if (mapping.size === 1) {
          "DISALLOWED_STD3_MAPPED"
        } else {
          "DISALLOWED_STD3_MAPPED_MULTI"
        }
    }

    /** Attempt to create a [[CodePointStatus]] value from the status string, the
      * mapping value, and the IDNA 2008 status value.
      */
    def from(value: String, mapping: MappingValue, idna2008Status: Option[IDNA2008Status]): Either[String, CodePointStatus] =
      value.trim match {
        case "valid" if mapping.value.isEmpty =>
          Right(Valid(idna2008Status))
        case "ignored" if idna2008Status.isEmpty && mapping.value.isEmpty =>
          Right(Ignored)
        case "mapped" if idna2008Status.isEmpty =>
          NonEmptyList.fromList(mapping.value).fold(
            Left("Mapped status must have at least one output code point."): Either[String, CodePointStatus]
          )(nel =>
            Right(Mapped(nel))
          )
        case "deviation" if idna2008Status.isEmpty =>
          Right(Deviation(mapping.value))
        case "disallowed" if idna2008Status.isEmpty && mapping.value.isEmpty => Right(Disallowed)
        case "disallowed_STD3_valid" if idna2008Status.isEmpty && mapping.value.isEmpty => Right(Disallowed_STD3_Valid)
        case "disallowed_STD3_mapped" if idna2008Status.isEmpty =>
          NonEmptyList.fromList(mapping.value).fold(
            Left("Disallowed_STD3_Mapped status must have at least one output code point."): Either[String, CodePointStatus]
          )(nel =>
            Right(Disallowed_STD3_Mapped(nel))
          )
        case _ => Left(s"Unknown or invalid row: Status=${value}, mapping=${mapping}, idna2008Status=${idna2008Status}")
      }
  }

  /** Newtype for an output mapping of code points. */
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

  /** Newtype for a comment string. */
  final case class Comment(value: String)

  object Comment {
    implicit val orderInstance: Order[Comment] =
      Order.by(_.value)

    implicit def orderingInstance: Ordering[Comment] = orderInstance.toOrdering
  }

  /** A type representing a single row parsed from the UTS-46 lookup tables.
    */
  final case class Row(
    casePattern: CasePattern,
    codePointStatus: CodePointStatus,
    comment: Option[Comment]
  ) {

    /** Convert a row into the case pattern and result string for the main code
      * point mapping function.
      */
    def asCase: (CasePattern, String) =
      comment.fold(
        (casePattern -> s"${codePointStatus.asCaseBody}\n")
      )(comment =>
        (casePattern -> s"${codePointStatus.asCaseBody} // ${comment.value}\n")
      )
  }

  object Row {
    implicit val orderInstance: Order[Row] =
      new Order[Row] {
        override def compare(x: Row, y: Row): Int =
          x.casePattern.compare(y.casePattern) match {
            case 0 =>
              x.codePointStatus.compare(y.codePointStatus) match {
                case 0 =>
                  x.comment.compare(y.comment)
                case otherwise =>
                  otherwise
              }
            case otherwise => otherwise
          }
      }

    implicit def orderingInstance: Ordering[Row] = orderInstance.toOrdering

    def apply(casePattern: CasePattern, codePointStatus: CodePointStatus, comment: Comment): Row =
      Row(casePattern, codePointStatus, Some(comment))

    // Regexes used to extract out a single input code point or a range of
    // input code points.
    val rangeRegex: Regex =
      """([0-9A-Fa-f]{1,6})\.\.([0-9A-Fa-f]{1,6})""".r
    val singleRegex: Regex =
      """([0-9A-Fa-f]{1,6})""".r

    /** Give an input code point string (the first column in the UTS-46 lookup
      * table), attempt to parse it into a `CasePattern`.
      */
    def parseInputCodePoints(value: String): Either[String, CasePattern] =
      value.trim match {
        case rangeRegex(lower, upper) =>
          CasePattern.fromHexStrings(lower, upper)
        case singleRegex(value) =>
          CasePattern.fromHexString(value)
        case _ =>
          Left(s"Unable to parse as code points: ${value}")
      }

    /** Given a single row/line in the UTS-46 lookup table, attempt to parse it
      * into a `Row` type.
      */
    def fromString(value: String): Either[String, Row] =
      value.split("""[;#]""").toList match {
        case input :: status :: comment :: Nil =>
          for {
            input <- parseInputCodePoints(input)
            status <- CodePointStatus.from(status, MappingValue.empty, None)
          } yield Row(input, status, Comment(comment))
        case input :: status :: mapping :: comment :: Nil =>
          for {
            input <- parseInputCodePoints(input)
            mapping <- MappingValue.fromString(mapping)
            status <- CodePointStatus.from(status, mapping, None)
          } yield Row(input, status, Comment(comment))
        case input :: status :: mapping :: idna2008Status :: comment :: Nil if mapping.trim.isEmpty =>
          for {
            input <- parseInputCodePoints(input)
            mapping <- MappingValue.fromString(mapping)
            idna2008Status <- IDNA2008Status.fromString(idna2008Status)
            status <- CodePointStatus.from(status, mapping, Some(idna2008Status))
          } yield Row(input, status, Comment(comment))
        case _ =>
          Left(s"Unable to parse row: ${value}")
      }
  }

  /** A type representing the parsed UTS-46 table.
    */
  final case class Rows(
    version: UnicodeVersion,
    rows: SortedSet[Row]
  ) {

    /** Convert the rows into cases for the main code point mapping function.
      */
    def rowsAsCases: SortedMap[CasePattern, String] = {
      rows.foldLeft(SortedMap.empty[CasePattern, String]){
        case (acc, row) =>
          acc + row.asCase
      }
    }

    /** Convert rows which represent code points which map to more than one output
      * code point into the appropriate case form for the secondary function
      * to lookup the code point mappings.
      *
      * For example, if a code point maps to a more than one output code
      * point, then the sentinel value MAPPED_MULTI_CODE_POINT is
      * returned. This is to avoid boxing in the common case where a code
      * point maps to exactly one output code point. When the sentinel value
      * MAPPED_MULTI_CODE_POINT is returned, then a secondary function is
      * called to lookup the List of output code points. This function
      * generates the cases for that secondary lookup table.
      */
    def mappedMultiCases: SortedMap[CasePattern, String] = {
      rows.foldLeft(SortedMap.empty[CasePattern, String]){
        case (acc, row) =>
          row.codePointStatus match {
            case CodePointStatus.Mapped(a) if a.size > 1 =>
              val commentString: String =
                row.comment.fold(
                  ""
                )(comment =>
                  s"// ${comment.value}"
                )
              acc + (row.casePattern -> s"""NonEmptyList.of(${a.map(a => printAsHex(a.value)).mkString_(", ")}) ${commentString}${"\n"}""")
            case _ =>
              acc
          }
      }
    }

    /** Convert rows which represent code points which map a single deviation code
      * point into the appropriate case form for the secondary function to
      * lookup these code point mappings.
      *
      * For example, if a code point maps to a single deviation code point,
      * then the sentinel value DEVIATION is returned. In the event that
      * `transitionalProcessing` is enabled then the code point is mapped and
      * a secondary function will be called to lookup that mapping. This
      * function generates the cases for that function.
      */
    def deviationMappedCases: SortedMap[CasePattern, String] = {
      rows.foldLeft(SortedMap.empty[CasePattern, String]){
        case (acc, row) =>
          row.codePointStatus match {
            case CodePointStatus.Deviation(List(mapping)) =>
              val commentString: String =
                row.comment.fold(
                  ""
                )(comment =>
                  s"// ${comment.value}"
                )
              acc + (row.casePattern -> s"${printAsHex(mapping.value)} ${commentString}\n")
            case _ =>
              acc
          }
      }
    }

    /** As [[#deviationMappedCases]], but for deviation code points which map to
      * ''more than one'' output code point. These are signaled by the
      * sentinel value DEVIATION_MULTI. The distinction is made so that the
      * deviation code points which map to a single output code point can just
      * return an `Int`, rather than a `List[Int]`, to avoid boxing.
      */
    def deviationMappedMultiCases: SortedMap[CasePattern, String] = {
      rows.foldLeft(SortedMap.empty[CasePattern, String]){
        case (acc, row) =>
          row.codePointStatus match {
            case CodePointStatus.Deviation(l) if l.size > 1 =>
              val commentString: String =
                row.comment.fold(
                  ""
                )(comment =>
                  s"// ${comment.value}"
                )
              acc + (row.casePattern -> s"""NonEmptyList.of(${l.map(a => printAsHex(a.value)).mkString(", ")}) ${commentString}${"\n"}""")
            case _ =>
              acc
          }
      }
    }

    /** Convert rows which represent code points which map to a single output code
      * point when UseSTD3ASCIIRules is false into the appropriate case form
      * for the secondary lookup function.
      *
      * For example, UseSTD3ASCIIRules is false and the sentinel
      * DISALLOWED_STD3_MAPPED value is returned from the main mapping
      * function, then the secondary function generated from these rows will be
      * called to lookup that mapping.
      */
    def disallowedSTD3MappedCases: SortedMap[CasePattern, String] = {
      rows.foldLeft(SortedMap.empty[CasePattern, String]){
        case (acc, row) =>
          row.codePointStatus match {
            case CodePointStatus.Disallowed_STD3_Mapped(mapping) if mapping.size === 1 =>
              val commentString: String =
                row.comment.fold(
                  ""
                )(comment =>
                  s"// ${comment.value}"
                )
              acc + (row.casePattern -> s"${printAsHex(mapping.head.value)} ${commentString}\n")
            case _ =>
              acc
          }
      }
    }

    /** As [[#disallowedSTD3Mapped]] but for code points which map to multiple
      * output code points. These are signaled by the sentinel value
      * DISALLOWED_STD3_MAPPED_MULTI. The distinction is made to permit the
      * secondary function for these code points which map to a single output
      * code point to return `Int`, as opposed to `List[Int]`, and avoid a
      * layer of boxing.
      */
    def disallowedSTD3MappedMultiCases: SortedMap[CasePattern, String] = {
      rows.foldLeft(SortedMap.empty[CasePattern, String]){
        case (acc, row) =>
          row.codePointStatus match {
            case CodePointStatus.Disallowed_STD3_Mapped(a) if a.size > 1 =>
              val commentString: String =
                row.comment.fold(
                  ""
                )(comment =>
                  s"// ${comment.value}"
                )
              acc + (row.casePattern -> s"""NonEmptyList.of(${a.map(a => printAsHex(a.value)).mkString_(", ")}) ${commentString}${"\n"}""")
            case _ =>
              acc
          }
      }
    }

    /** Given a package name, generate the `String` content of the generated
      * source file.
      */
    def asSourceFile: String = {
      // Approximately 9000 lines in the 14.0.0 version
      val sb: StringBuilder = new StringBuilder(9000 * 3)

      val prelude: String = s"""package org.typelevel.idna4s.core.uts46
                               |
                               |import scala.annotation.switch
                               |import cats.data._
                               |
                               |// THIS FILE IS GENERATED, DO NOT MODIFY BY HAND
                               |// These mapping tables conform to Unicode version ${version.value}, which is backwards compatible.
                               |
                               |private[uts46] trait GeneratedCodePointMapper extends CodePointMapperBase {
                               |""".stripMargin

      sb.append(prelude)

      // Generate the main mapping function
      generateLookupTableMethods(Config.default("unsafeMapIntCodePointSentinel"), rowsAsCases, sb)

      // Generate the secondary mapping functions
      generateLookupTableMethods(Config.default("unsafeLookupDeviationCodePointMapping"), deviationMappedCases, sb)
      generateLookupTableMethods(Config.default("unsafeLookupDisallowedSTD3CodePointMapping"), disallowedSTD3MappedCases, sb)
      generateLookupTableMethods(Config.defaultList("unsafeLookupDeviationCodePointMultiMapping"), deviationMappedMultiCases, sb)
      generateLookupTableMethods(Config.defaultList("unsafeLookupDisallowedSTD3CodePointMultiMapping"), disallowedSTD3MappedMultiCases, sb)
      generateLookupTableMethods(Config.defaultList("unsafeLookupCodePointMultiMapping"), mappedMultiCases, sb)

      sb.append("}\n").toString
    }
  }

  object Rows {

    // Regex for parsing the unicode version string from the file.
    val unicodeVersionRegex: Regex =
      """#\s*Version:\s*(\d+\.\d+\.\d+)""".r

    /** Parse the rows from a list of lines. */
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

      type F[A] = Either[String, A]

      parseVersion(lines) match {
        case Left(error) => Left(error)
        case Right((version, rest)) =>
          // Drop comments after parsing the version
          rest.dropWhile(_.startsWith("#")).foldM[F, SortedSet[Row]](SortedSet.empty[Row]){
            case (acc, value) if value.trim.isEmpty || value.trim.startsWith("#") =>
              Right(acc)
            case (acc, value) =>
              Row.fromString(value).map(acc + _).leftMap(error =>
                s"Error at $value: $error"
              )
          }.map(rows => Rows(version, rows))
      }
    }

    /** Download the UTS-46 lookup table from the given URL and parse it into
      * rows.
      */
    def fromURL(url: String): Try[Rows] =
      Try(new URL(url)).flatMap(url =>
        Try(IO.readLinesURL(url))
      ).flatMap(lines =>
        fromLines(lines).fold(
          e => Failure(new RuntimeException(e)),
          rows => Success(rows)
        )
      )

    /** Generate the mapping table code by downloading the mappings from
      * `www.unicode.org`.
      *
      * @param version The version of Unicode to use to generate the mapping
      *        table code, if `None`, then "latest" will be used. This is the
      *        recommended usage as Unicode will post pre-release versions on
      *        their site that we probably don't want to implement a release
      *        against.
      */
    def fromUnicodeURL(version: Option[UnicodeVersion]): Try[Rows] = {
      def makeUrl(rawVersion: String): String =
        s"https://www.unicode.org/Public/idna/${rawVersion}/IdnaMappingTable.txt"

      val url: String =
        version.fold(
          makeUrl("latest")
        )(version =>
          makeUrl(version.value)
        )

      fromURL(url).flatMap(rows =>
        // Validate that if an explicit version was specified, that is what we
        // parsed.
        version.fold(
          Success(rows): Try[Rows]
        )(version =>
          if (rows.version == version) {
            Success(rows)
          } else {
            Failure(new RuntimeException(s"Expected to get the mapping table for version ${version}, but got ${rows.version}."))
          }
        )
      )
    }

    /** Generate the mapping table code by downloading the mappings from
      * `www.unicode.org`. The "latest" version of Unicode will be used, and
      * parsed from the file.
      */
    def fromUnicodeURL: Try[Rows] =
      fromUnicodeURL(None)

    /** Generate the mapping table code by downloading the mappings from
      * `www.unicode.org`.
      *
      * @param version The version of Unicode to use to generate the mapping
      *        table code.
      */
    def fromUnicodeURL(version: UnicodeVersion): Try[Rows] =
      fromUnicodeURL(Some(version))
  }

  /** Generate the UTS-46 lookup table code.
    *
    * @param rows The parsed rows representing the source UTS-46 lookup tables
    *        for step 1 of processing.
    *
    * @param dir The base directory that the generated file will be in.
    */
  def generateFromRows(rows: Rows, dir: File): File = {
    val outputFile: File =
      dir / "org" / "typelevel" / "idna4s" / "core" / "uts46" / "GeneratedCodePointMapper.scala"

    IO.write(outputFile, rows.asSourceFile)

    outputFile
  }

  /** Download the UTS-46 lookup table sources and generate the UTS-46 lookup
    * table code. This will use the "latest" release from Unicode.
    *
    * @param dir The base directory that the generated file will be in.
    */
  def generate(dir: File): Seq[File] =
    Rows.fromUnicodeURL.fold(
      e => throw e,
      rows => List(generateFromRows(rows, dir))
    )
}
