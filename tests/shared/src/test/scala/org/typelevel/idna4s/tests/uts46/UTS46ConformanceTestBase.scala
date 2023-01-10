package org.typelevel.idna4s.tests.uts46

import cats._
import cats.data._
import cats.derived.semiauto
import cats.syntax.all._
import java.lang.StringBuilder
import java.nio.charset.StandardCharsets.UTF_8
import java.util.SortedMap
import java.util.regex.MatchResult
import org.typelevel.idna4s.core._
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.util.matching.Regex

trait UTS46ConformanceTestBase {
  protected def testLines: SortedSet[String]
}

object UTS46ConformanceTestBase {
  final case class ConformanceTest(
    source: String,
    toUnicodeResult: String,
    toUnicodeStatus: SortedSet[Status],
    toAsciiN: String,
    toAsciiNStatus: SortedSet[Status],
    toAsciiT: String,
    toAsciiTStatus: SortedSet[Status],
    comment: String
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

    def stringToStatusSet(value: String)

    private[this] final val LineRegex =
      """\s*(.*)\s*;\s*(.*)\s*;\s*(.*)\s*;\s*(.*)\s*;\s*(.*)\s*;\s*(.*)\s*;\s*(.*)\s*#\s*(.*)""".r

    def fromLine(value: String): Either[String, ConformanceTest] =
      value match {
        case LineRegex(source, toUnicodeResult, toUnicodeStatus, toAsciiN, toAsciiNStatus, toAsciiT, toAsciiTStatus, comment) =>
          (unescape(source), unescape(toUnicodeResult), unescape(toUnicodeStatus), unescape(toAsciiN), unescape(toAsciiNStatus), unescape(toAsciiT), unescape(toAsciiTStatus)).mapN{
            case (source, toUnicodeResult, toUnicodeStatus, toAsciiN, toAsciiNStatus, toAsciiT, toAsciiTStatus) =>
              ConformanceTest(source, toUnicodeResult, toUnicodeStatus, toAsciiN, toAsciiNStatus, toAsciiT, toAsciiTStatus, comment)
          }
        case _ =>
          Left(s"Value is not a valid conformance test line: ${value}")
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
              x.step.compare(y.step)
            case (x: Validity, y: Validity) =>
              x.step.compare(y.step)
            case (x: UseSTD3ASCIIRules, y: UseSTD3ASCIIRules) =>
              x.step.compare(y.step)
            case (x: ToASCII, y: ToASCII) =>
              x.step.compare(y.step)
            case (x: Bidi, y: Bidi) =>
              x.step.compare(y.step)
            case (x: ContextJ, y: ContextJ) =>
              x.step.compare(y.step)
            case (x: ToUnicode, y: ToUnicode) =>
              x.step.compare(y.step)
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

    final case class Processing(step: Long) extends Status
    final case class Validity(step: Long) extends Status
    final case class UseSTD3ASCIIRules(step: Long) extends Status
    final case class ToASCII(step: Long) extends Status
    final case class Bidi(step: Long) extends Status
    final case class ContextJ(step: Long) extends Status
    final case class ToUnicode(step: Long) extends Status

    private[this] final val StatusRegex =
      """(P|V|U|A|B|C|X)(\d+)""".r

    def fromString(value: String): Either[String, String] =
      value.trim match {
        case StatusRegex("P", step) =>
          Either.catchNonFatal(step.toLong).map(step =>
            Right(Processing(step))
          )
        case StatusRegex("V", step) =>
          Either.catchNonFatal(step.toLong).map(step =>
            Right(Validity(step))
          )
        case StatusRegex("U", step) =>
          Either.catchNonFatal(step.toLong).map(step =>
            Right(UseSTD3ASCIIRules(step))
          )
        case StatusRegex("A", step) =>
          Either.catchNonFatal(step.toLong).map(step =>
            Right(ToASCII(step))
          )
        case StatusRegex("B", step) =>
          Either.catchNonFatal(step.toLong).map(step =>
            Right(Bidi(step))
          )
        case StatusRegex("C", step) =>
          Either.catchNonFatal(step.toLong).map(step =>
            Right(ContextJ(step))
          )
        case StatusRegex("X", step) =>
          Either.catchNonFatal(step.toLong).map(step =>
            Right(ToUnicode(step))
          )
        case _ =>
          Left(s"Not a valid UTS-46 conformance status: ${value}")
      }
  }
}
