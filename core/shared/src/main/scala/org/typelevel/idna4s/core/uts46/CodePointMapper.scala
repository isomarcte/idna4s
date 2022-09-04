package org.typelevel.idna4s.core.uts46

import scala.annotation.tailrec
import scala.annotation.switch
import cats.data._
import org.typelevel.idna4s.core._
import scala.util.control.NoStackTrace
import java.lang.StringBuilder

trait CodePointMapper extends Serializable {
  import CodePointMapper._
  import CodePointStatus._

  def mapCodePoints(useStd3ASCIIRules: Boolean, transitionalProcessing: Boolean)(input: String): Either[MappingFailure, String] = {
    val len: Int = input.length

    @tailrec
    def loop(acc: StringBuilder, errors: List[MappingError], index: Int): Either[MappingFailure, String] =
      if (index >= len) {
        NonEmptyList.fromList(errors).fold(
          Right(acc.toString): Either[MappingFailure, String]
        )(errors =>
          Left(MappingFailure(errors, acc.toString))
        )
      } else {
        val value: Int = input.codePointAt(index)
        val indexIncrement: Int = if (value >= 0x10000) 2 else 1

        (unsafeMapIntCodePointSentinel(value): @switch) match {
          case -1 | -2 | -3 => // VALID | VALID_NV8 | VALID_XV8
            loop(acc.appendCodePoint(value), errors, index + indexIncrement)
          case -4 => // IGNORED
            loop(acc, errors, index + indexIncrement)
          case -5 => // MAPPED_MULTI_CODE_POINT
            loop(unsafeLookupCodePointMultiMapping(value).foldLeft(acc){
              case (acc, value) =>
                acc.appendCodePoint(value)
            }, errors, index + indexIncrement)
          case -6 => // DEVIATION
            if (transitionalProcessing) {
              loop(acc.appendCodePoint(unsafeLookupDeviationCodePointMapping(value)), errors, index + indexIncrement)
            } else {
              loop(acc.appendCodePoint(value), errors, index + indexIncrement)
            }
          case -7 => // DEVIATION_MULTI
            if (transitionalProcessing) {
              loop(unsafeLookupDeviationCodePointMultiMapping(value).foldLeft(acc){
                case (acc, value) =>
                  acc.appendCodePoint(value)
              }, errors, index + indexIncrement)
            } else {
              loop(acc.appendCodePoint(value), errors, index + indexIncrement)
            }
          case -8 => // DISALLOWED
            loop(acc.appendCodePoint(ReplacementCharacter), MappingError(index, "Disallowed code point in input.", CodePoint.unsafeFromInt(value)) :: errors, index + indexIncrement)
          case -9 => // DISALLOWED_STD3_VALID
            if (useStd3ASCIIRules) {
              loop(acc.appendCodePoint(ReplacementCharacter), MappingError(index, "Disallowed code point in input.", CodePoint.unsafeFromInt(value)) :: errors, index + indexIncrement)
            } else {
              loop(acc.appendCodePoint(value), errors, index + indexIncrement)
            }
          case -10 => // DISALLOWED_STD3_MAPPED
            if (useStd3ASCIIRules) {
              loop(acc.appendCodePoint(ReplacementCharacter), MappingError(index, "Disallowed code point in input.", CodePoint.unsafeFromInt(value)) :: errors, index + indexIncrement)
            } else {
              loop(acc.appendCodePoint(unsafeLookupDisallowedSTD3CodePointMapping(value)), errors, index + indexIncrement)
              }
          case -11 => // DISALLOWED_STD3_MAPPED_MULTI
            if (useStd3ASCIIRules) {
              loop(acc.appendCodePoint(ReplacementCharacter), MappingError(index, "Disallowed code point in input.", CodePoint.unsafeFromInt(value)) :: errors, index + indexIncrement)
            } else {
              loop(unsafeLookupDisallowedSTD3CodePointMultiMapping(value).foldLeft(acc){
                case (acc, value) =>
                  acc.appendCodePoint(value)
              }, errors, index + indexIncrement)
            }
          case -12 => // DEVIATION_IGNORED
            loop(acc, errors, index + indexIncrement)
          case otherwise => // MAPPED or bug
            if (otherwise < 0) {
              throw new AssertionError(s"Unexpected mapping result (this is probably a bug in idna4s): $otherwise")
            } else {
              // This is a mapped code point
              loop(acc.appendCodePoint(otherwise), errors, index + indexIncrement)
            }
        }
      }

    loop(new StringBuilder(len), Nil, 0)
  }

  protected def unsafeMapIntCodePointSentinel(codePoint: Int): Int

  protected def unsafeLookupCodePointMultiMapping(codePoint: Int): NonEmptyList[Int]

  protected def unsafeLookupDeviationCodePointMapping(deviationCodePoint: Int): Int

  protected def unsafeLookupDeviationCodePointMultiMapping(deviationCodePoint: Int): NonEmptyList[Int]

  protected def unsafeLookupDisallowedSTD3CodePointMapping(disallowedSTD3CodePoint: Int): Int

  protected def unsafeLookupDisallowedSTD3CodePointMultiMapping(disallowedSTD3CodePoint: Int): NonEmptyList[Int]

  // final //

  final def mapCodePoint(codePoint: CodePoint): CodePointStatus =
    (unsafeMapIntCodePointSentinel(codePoint.value): @switch) match {
      // Literals used rather than named constants because scalac won't
      // generate a switch with the named constants.
      case -1 => // VALID
          Valid.always
      case -2 => // VALID_NV8
        Valid.nv8
      case -3 => // VALID_XV8
        Valid.xv8
      case -4 => // IGNORED
        Ignored.instance
      case -5 => // MAPPED_MULTI_CODE_POINT
        Mapped.of(unsafeLookupCodePointMultiMapping(codePoint.value).map(CodePoint.unsafeFromInt))
      case -6 => // DEVIATION
        Deviation.one(CodePoint.unsafeFromInt(unsafeLookupDeviationCodePointMapping(codePoint.value)))
      case -7 => // DEVIATION_MULTI
        Deviation.of(unsafeLookupDeviationCodePointMultiMapping(codePoint.value).map(CodePoint.unsafeFromInt).toList)
      case -8 => // DISALLOWED
        Disallowed.instance
      case -9 => // DISALLOWED_STD3_VALID
        Disallowed_STD3_Valid.instance
      case -10 => // DISALLOWED_STD3_MAPPED
        Disallowed_STD3_Mapped.one(CodePoint.unsafeFromInt(unsafeLookupDisallowedSTD3CodePointMapping(codePoint.value)))
      case -11 => // DISALLOWED_STD3_MAPPED_MULTI
        Disallowed_STD3_Mapped.of(unsafeLookupDisallowedSTD3CodePointMultiMapping(codePoint.value).map(CodePoint.unsafeFromInt))
      case -12 => // DEVIATION_IGNORED
        Deviation.ignored
      case otherwise =>
        // Mapped. There is no sentinel value for mapped, because in that a
        // code point maps to a single new code point, we just return the
        // mapping directly.
        Mapped.one(CodePoint.unsafeFromInt(otherwise))
    }

  final def unsafeMapIntCodePoint(codePoint: Int): Either[String, CodePointStatus] =
    CodePoint.fromInt(codePoint).map(mapCodePoint)
}

object CodePointMapper {
  private[uts46] val VALID: Int = -1
  private[uts46] val VALID_NV8: Int = -2
  private[uts46] val VALID_XV8: Int = -3
  private[uts46] val IGNORED: Int = -4
  private[uts46] val MAPPED_MULTI_CODE_POINT = -5
  private[uts46] val DEVIATION = -6
  private[uts46] val DEVIATION_MULTI = -7
  private[uts46] val DISALLOWED = -8
  private[uts46] val DISALLOWED_STD3_VALID = -9
  private[uts46] val DISALLOWED_STD3_MAPPED = -10
  private[uts46] val DISALLOWED_STD3_MAPPED_MULTI = -11
  private[uts46] val DEVIATION_IGNORED = -12

  private val ReplacementCharacter: Int =
    0xFFFD

  sealed abstract class MappingError extends Serializable {
    def failureIndex: Int

    def message: String

    def codePoint: CodePoint

    override final def toString: String =
      s"MappingError(message = $message, failureIndex = $failureIndex, codePoint = $codePoint)"
  }

  object MappingError {
    private[this] final case class MappingErrorImpl(override val failureIndex: Int, override val message: String, override val codePoint: CodePoint) extends MappingError

    def apply(failureIndex: Int, message: String, codePoint: CodePoint): MappingError =
      MappingErrorImpl(failureIndex, message, codePoint)
  }

  sealed abstract class MappingFailure extends RuntimeException with NoStackTrace {
    def errors: NonEmptyList[MappingError]

    def partiallyMappedInput: String

    override final def getMessage: String =
      toString

    override final def toString: String =
      s"MappingFailure(errors = ${errors}, partiallyMappedInput = ${partiallyMappedInput})"
  }

  object MappingFailure {
    private[this] final case class MappingFailureImpl(override val errors: NonEmptyList[MappingError], override val partiallyMappedInput: String) extends MappingFailure

    def apply(errors: NonEmptyList[MappingError], partiallyMappedInput: String): MappingFailure =
      MappingFailureImpl(errors, partiallyMappedInput)
  }
}
