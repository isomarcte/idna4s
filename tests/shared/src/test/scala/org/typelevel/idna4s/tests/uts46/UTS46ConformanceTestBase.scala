package org.typelevel.idna4s.tests.uts46

import cats._
import cats.data._
import cats.derived.semiauto
import cats.syntax.all._
import java.util.regex.MatchResult
import org.typelevel.idna4s.core._
import scala.collection.immutable.SortedSet
import scala.util.matching.Regex

private[uts46] trait UTS46ConformanceTestBase {
  protected def testLines: SortedSet[String]
}

private[uts46] object UTS46ConformanceTestBase {
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
      """\\x\{(\p{XDigit}{4})\})""".r

    def unescape(value: String): Either[String, CodePoint] = {

    }

    private[this] final val LineRegex =
      """\s*(.*)\s*;\s*(.*)\s*;\s*(.*)\s*;\s*(.*)\s*;\s*(.*)\s*;\s*(.*)\s*;\s*(.*)\s*""".r

    def fromLine(value: String): Either[String, ConformanceTest] =
      value match {
        case LineRegex(source, toUnicodeResult, toUnicodeStatus, toAsciiN, toAsciiNStatus, toAsciiT, toAsciiTStatus) =>
          ???
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
  }
}
