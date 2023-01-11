package org.typelevel.idna4s.tests.uts46

import cats._
import cats.data._
import cats.derived.semiauto
import cats.syntax.all._
import java.lang.StringBuilder
import org.typelevel.idna4s.core._
import org.typelevel.idna4s.tests.uts46.ConformanceTest.Status
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

final case class ConformanceTest(
  source: String,
  toUnicodeResult: String,
  toUnicodeStatus: SortedSet[Status],
  toAsciiNResult: String,
  toAsciiNStatus: SortedSet[Status],
  toAsciiTResult: String,
  toAsciiTStatus: SortedSet[Status],
  comment: Option[String]
)

object ConformanceTest {

  implicit val hashAndOrderForConformanceTest: Hash[ConformanceTest] with Order[ConformanceTest] = {
    val order: Order[ConformanceTest] =
      semiauto.order

    new Hash[ConformanceTest] with Order[ConformanceTest] {
      override def hash(x: ConformanceTest): Int =
        x.hashCode

      override def compare(x: ConformanceTest, y: ConformanceTest): Int =
        order.compare(x, y)
    }
  }

  implicit def orderingInstance: Ordering[ConformanceTest] =
    hashAndOrderForConformanceTest.toOrdering

  private[this] final val UnescapeRegex0 =
    """\\u(\p{XDigit}{4})""".r

  private[this] final val UnescapeRegex1 =
    """\\x\{(\p{XDigit}{4})\}""".r

  private[this] sealed abstract class UnescapeState extends Product with Serializable {
    import UnescapeState._

    final def unwindIntoStringBuilder(sb: StringBuilder): StringBuilder =
      this match {
        case StartEscape =>
          sb.append('\\')
        case ExpectLeftBrace =>
          sb.append('\\').append('x')
        case ExpectRightBrace(chars) =>
          // Unwinding is the same in this case
          ExpectHexChar(false, chars).unwindIntoStringBuilder(sb)
        case ExpectHexChar(false, chars) =>
          chars.foldLeft(sb.append('\\').append('x').append('{')){
            case (sb, c) =>
              sb.append(c)
          }
        case ExpectHexChar(true, chars) =>
          chars.foldLeft(sb.append('\\').append('u')){
            case (sb, c) =>
              sb.append(c)
          }
      }
  }

  private[this] object UnescapeState {
    case object StartEscape extends UnescapeState
    case object ExpectLeftBrace extends UnescapeState
    final case class ExpectRightBrace(hexDigits: Chain[Char]) extends UnescapeState
    final case class ExpectHexChar(isUStyleEscape: Boolean, hexDigits: Chain[Char]) extends UnescapeState

    implicit val hashAndOrderForUnescapeState: Hash[UnescapeState] with Order[UnescapeState] =
      new Hash[UnescapeState] with Order[UnescapeState] {
        override def hash(x: UnescapeState): Int =
          x.hashCode

        override def compare(x: UnescapeState, y: UnescapeState): Int =
          (x, y) match {
            case (StartEscape, StartEscape) => 0
            case (ExpectLeftBrace, ExpectLeftBrace) => 0
            case (ExpectRightBrace(x), ExpectRightBrace(y)) => x.compare(y)
            case (ExpectHexChar(a, b), ExpectHexChar(c, d)) => (a, b).compare((c, d))
            case (StartEscape, _) => 1
            case (_, StartEscape) => -1
            case (ExpectLeftBrace, _) => 1
            case (_, ExpectLeftBrace) => -1
            case (_: ExpectRightBrace, _) => 1
            case (_, _: ExpectRightBrace) => -1
          }
      }
  }

  def unescape(value: String): Either[String, String] = {
    @tailrec
    def loop(acc: StringBuilder, state: Option[UnescapeState], rest: List[Char]): Either[String, String] =
      rest match {
        case Nil =>
          val sb =
            state match {
              case None =>
                acc
              case Some(state) =>
                // Assume if state is non-empty, then we aren't in an escape and unwind
                state.unwindIntoStringBuilder(acc)
            }

          Right(sb.toString)
        case '\\' :: rest =>
          state match {
            case None =>
              loop(acc, Some(UnescapeState.StartEscape), rest)
            case Some(state) =>
              // Either this is a bug, the input is corrupt, or we are not
              // in an escape sequence. We assume the final case.
              loop(state.unwindIntoStringBuilder(acc), Some(UnescapeState.StartEscape), rest)
          }
        case x :: rest =>
          state match {
            case None =>
              loop(acc.append(x), state, rest)
            case Some(state) =>
              state match {
                case UnescapeState.StartEscape =>
                  x match {
                    case 'u' =>
                      loop(acc, Some(UnescapeState.ExpectHexChar(true, Chain.empty)), rest)
                    case 'x' =>
                      loop(acc, Some(UnescapeState.ExpectLeftBrace), rest)
                    case x =>
                      // Assume it this is not actually an escape sequence
                      loop(state.unwindIntoStringBuilder(acc).append(x), None, rest)
                  }
                case UnescapeState.ExpectLeftBrace =>
                  x match {
                    case '{' =>
                      loop(acc, Some(UnescapeState.ExpectHexChar(false, Chain.empty)), rest)
                    case x =>
                      // Assume it this is not actually an escape sequence
                      loop(state.unwindIntoStringBuilder(acc).append(x), None, rest)
                  }
                case UnescapeState.ExpectRightBrace(chars) =>
                  x match {
                    case '}' =>
                      CodePoint.fromString(chars.mkString_("0x", "", "")) match {
                        case Right(value) =>
                          loop(acc.appendCodePoint(value.value), None, rest)
                        case Left(error) =>
                          Left(error)
                      }
                    case x =>
                      // Assume it this is not actually an escape sequence
                      loop(state.unwindIntoStringBuilder(acc).append(x), None, rest)
                  }
                case UnescapeState.ExpectHexChar(isUStyleEscape, hexDigits) =>
                  x match {
                    case x if (x >= 'a' && x <= 'f') || (x >= 'A' && x <= 'F') || (x >= '0' && x <= '9') =>
                      val state = (hexDigits :+ x) match {
                        case hexDigits =>
                          if (hexDigits.size >= 3) {
                            UnescapeState.ExpectRightBrace(hexDigits)
                          } else {
                            UnescapeState.ExpectHexChar(isUStyleEscape, hexDigits)
                          }
                      }
                      loop(acc, Some(state), rest)
                    case x =>
                      // Assume it this is not actually an escape sequence
                      loop(state.unwindIntoStringBuilder(acc).append(x), None, rest)
                  }
              }
          }
      }

    loop(new StringBuilder(value.size), None, value.toList)
  }

  def stringToStatusSet(value: String): Either[Throwable, Option[SortedSet[Status]]] = {
    def error: Either[Throwable, Option[SortedSet[Status]]] = Left(new RuntimeException(s"Not a valid UTS-46 conformance test status string: ${value}"))
    value.trim match {
      case value if value.isEmpty =>
        Option.empty[SortedSet[Status]].asRight[Throwable]
      case value =>
        if (value.nonEmpty && value.head === '[') {
          value.tail match {
            case value =>
              if (value.trim.size > 1 && value.last === ']') {
                value.init.split(',').toList.foldMapM(value =>
                  Status.fromString(value.trim).map((value: Status) => SortedSet(value))
                ).map(value => Option(value))
              } else if (value.trim === "]"){
                Option.empty[SortedSet[Status]].asRight[Throwable]
              } else {
                error
              }
          }
        } else {
          error
        }
    }
  }

  private[this] final val LineRegex =
    """\s*([^;]*)\s*;\s*([^;]*)\s*;\s*([^;]*)\s*;\s*([^;]*)\s*;\s*([^;]*)\s*;\s*([^;]*)\s*;\s*([^#]*)\s*(#.*)?""".r

  def fromLine(value: String): Either[Throwable, ConformanceTest] = {
    def unescapeThrowable(value: String): Either[Throwable, String] =
      unescape(value).leftMap(error => new RuntimeException(error))

    value match {
      case LineRegex(source, toUnicodeResult, toUnicodeStatus, toAsciiNResult, toAsciiNStatus, toAsciiTResult, toAsciiTStatus, comment) =>
        (unescapeThrowable(source), unescapeThrowable(toUnicodeResult), unescapeThrowable(toUnicodeStatus), unescapeThrowable(toAsciiNResult), unescapeThrowable(toAsciiNStatus), unescapeThrowable(toAsciiTResult), unescapeThrowable(toAsciiTStatus)).tupled.flatMap{
          case (source, toUnicodeResult, toUnicodeStatus, toAsciiNResult, toAsciiNStatus, toAsciiTResult, toAsciiTStatus) =>
            (stringToStatusSet(toUnicodeStatus), stringToStatusSet(toAsciiNStatus), stringToStatusSet(toAsciiTStatus)).mapN{
              case (toUnicodeStatus, toAsciiNStatus, toAsciiTStatus) =>
                // Empty toUnicodeStatus means no errors
                // Empty toAsciiNStatus means same as toUnicodeStatus
                // Empty toAsciiTStatus means same as toAsciiNStatus

                val unicodeStatus: SortedSet[Status] = toUnicodeStatus.getOrElse(SortedSet.empty[Status])
                val asciiNStatus: SortedSet[Status] = toAsciiNStatus.getOrElse(unicodeStatus)
                val asciiTStatus: SortedSet[Status] = toAsciiTStatus.getOrElse(asciiNStatus)

                val c: Option[String] =
                  if (comment.trim.isEmpty || comment.trim === "#") {
                    None
                  } else {
                    Some(comment.trim.dropWhile(_ === '#'))
                  }

                ConformanceTest(source, toUnicodeResult, unicodeStatus, toAsciiNResult, asciiNStatus, toAsciiTResult, asciiTStatus, c)
            }.leftMap(statusError =>
              new RuntimeException(s"Error when parsing one of the status lines in $value", statusError)
            )
        }
      case _ =>
        Left(new RuntimeException(s"Value is not a valid conformance test line: ${value}"))
    }
  }

  sealed abstract class Status extends Product with Serializable

  object Status {
    implicit val hashAndOrderForStatus: Hash[Status] with Order[Status] =
      new Hash[Status] with Order[Status] {
        override final def hash(x: Status): Int = x.hashCode

        override final def compare(x: Status, y: Status): Int =
          (x, y) match {
            case (x: Processing, y: Processing) =>
              (x.section, x.subSection).compare((y.section, y.subSection))
            case (x: Validity, y: Validity) =>
              (x.section, x.subSection).compare((y.section, y.subSection))
            case (x: UseSTD3ASCIIRules, y: UseSTD3ASCIIRules) =>
              (x.section, x.subSection).compare((y.section, y.subSection))
            case (x: ToASCII, y: ToASCII) =>
              (x.section, x.subSection).compare((y.section, y.subSection))
            case (x: Bidi, y: Bidi) =>
              (x.section, x.subSection).compare((y.section, y.subSection))
            case (x: ContextJ, y: ContextJ) =>
              (x.section, x.subSection).compare((y.section, y.subSection))
            case (x: ToUnicode, y: ToUnicode) =>
              (x.section, x.subSection).compare((y.section, y.subSection))
            case (_: Processing, _) =>
              1
            case (_, _: Processing) =>
              -1
            case (_: Validity, _) =>
              1
            case (_, _: Validity) =>
              -1
            case (_: UseSTD3ASCIIRules, _) =>
              1
            case (_, _: UseSTD3ASCIIRules) =>
              -1
            case (_: ToASCII, _) =>
              1
            case (_, _: ToASCII) =>
              -1
            case (_: Bidi, _) =>
              1
            case (_, _: Bidi) =>
              -1
            case (_: ContextJ, _) =>
              1
            case (_, _: ContextJ) =>
              -1
          }
      }

    implicit def orderingInstance: Ordering[Status] =
      hashAndOrderForStatus.toOrdering

    final case class Processing(section: Long, subSection: Option[Long]) extends Status
    final case class Validity(section: Long, subSection: Option[Long]) extends Status
    final case class UseSTD3ASCIIRules(section: Long, subSection: Option[Long]) extends Status
    final case class ToASCII(section: Long, subSection: Option[Long]) extends Status
    final case class Bidi(section: Long, subSection: Option[Long]) extends Status
    final case class ContextJ(section: Long, subSection: Option[Long]) extends Status
    final case class ToUnicode(section: Long, subSection: Option[Long]) extends Status

    private[this] final val StatusRegex =
      """(P|V|U|A|B|C|X)(\d+)(_\d+)?""".r

    def fromString(value: String): Either[Throwable, Status] =
      value.trim match {
        case StatusRegex(category, section, subSection) =>
          (Either.catchNonFatal(section.toLong), Option(subSection).traverse(subSection => Either.catchNonFatal(subSection.tail.toLong))).tupled.flatMap{
            case (section, subSection) =>
              category match {
                case "P" =>
                  Processing(section, subSection).asRight[Throwable]
                case "V" =>
                  Validity(section, subSection).asRight[Throwable]
                case "U" =>
                  UseSTD3ASCIIRules(section, subSection).asRight[Throwable]
                case "A" =>
                  ToASCII(section, subSection).asRight[Throwable]
                case "B" =>
                  Bidi(section, subSection).asRight[Throwable]
                case "C" =>
                  ContextJ(section, subSection).asRight[Throwable]
                case "X" =>
                  ToUnicode(section, subSection).asRight[Throwable]
                case otherwise =>
                  Left(new RuntimeException(s"Unknown status category: ${otherwise}"))
              }
          }
        case _ =>
          Left(new RuntimeException(s"Not a valid UTS-46 conformance status: ${value}"))
      }
  }
}
