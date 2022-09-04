package org.typelevel.idna4s.core.uts46

import scala.annotation.tailrec
import scala.annotation.switch
import cats.data._
import org.typelevel.idna4s.core._
import scala.util.control.NoStackTrace
import java.lang.StringBuilder

private[uts46] trait CodePointMapperBase {
  protected final val VALID: Int = -1
  protected final val VALID_NV8: Int = -2
  protected final val VALID_XV8: Int = -3
  protected final val IGNORED: Int = -4
  protected final val MAPPED_MULTI_CODE_POINT = -5
  protected final val DEVIATION = -6
  protected final val DEVIATION_MULTI = -7
  protected final val DISALLOWED = -8
  protected final val DISALLOWED_STD3_VALID = -9
  protected final val DISALLOWED_STD3_MAPPED = -10
  protected final val DISALLOWED_STD3_MAPPED_MULTI = -11
  protected final val DEVIATION_IGNORED = -12

  protected def unsafeMapIntCodePointSentinel0(codePoint: Int): Int

  protected def unsafeLookupCodePointMultiMapping0(codePoint: Int): NonEmptyList[Int]

  protected def unsafeLookupDeviationCodePointMapping0(deviationCodePoint: Int): Int

  protected def unsafeLookupDeviationCodePointMultiMapping0(deviationCodePoint: Int): NonEmptyList[Int]

  protected def unsafeLookupDisallowedSTD3CodePointMapping0(disallowedSTD3CodePoint: Int): Int

  protected def unsafeLookupDisallowedSTD3CodePointMultiMapping0(disallowedSTD3CodePoint: Int): NonEmptyList[Int]
}

object CodePointMapper extends GeneratedCodePointMapper {

  /** Map the code points in the given `String` according to the UTS-46 mapping tables
    * as described in section 5 of UTS-46. useStd3ASCIIRules is true and
    * transitionalProcessing is false.
    */
  def mapCodePoints(input: String): Either[MappingFailure, String] =
    mapCodePoints(true, false)(input)

  /** Map the code points in the given `String` according to the UTS-46 mapping tables
    * as described in section 5 of UTS-46. This is step 1 of processing an input
    *`String` according to UTS-46 for IDNA compatibility.
    *
    * @param useStd3ASCIIRules Whether or not to use STD3 ASCII rules. UTS-46
    *        strongly recommends this be `true`.
    * @param transitionalProcessing Determines if the deviation characters
    *        should be mapped or considered valid.  From UTS-46, "Transitional
    *        Processing should only be used immediately before a DNS lookup in
    *        the circumstances where the registry does not guarantee a
    *        strategy of bundling or blocking. Nontransitional Processing,
    *        which is fully compatible with IDNA2008, should be used in all
    *        other cases.
    *
    * @see [[https://www.unicode.org/reports/tr46/#IDNA_Mapping_Table UTS-46 Section 5]]
    */
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

        (unsafeMapIntCodePointSentinel0(value): @switch) match {
          case -1 | -2 | -3 => // VALID | VALID_NV8 | VALID_XV8
            loop(acc.appendCodePoint(value), errors, index + indexIncrement)
          case -4 => // IGNORED
            loop(acc, errors, index + indexIncrement)
          case -5 => // MAPPED_MULTI_CODE_POINT
            loop(unsafeLookupCodePointMultiMapping0(value).foldLeft(acc){
              case (acc, value) =>
                acc.appendCodePoint(value)
            }, errors, index + indexIncrement)
          case -6 => // DEVIATION
            if (transitionalProcessing) {
              loop(acc.appendCodePoint(unsafeLookupDeviationCodePointMapping0(value)), errors, index + indexIncrement)
            } else {
              loop(acc.appendCodePoint(value), errors, index + indexIncrement)
            }
          case -7 => // DEVIATION_MULTI
            if (transitionalProcessing) {
              loop(unsafeLookupDeviationCodePointMultiMapping0(value).foldLeft(acc){
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
              loop(acc.appendCodePoint(unsafeLookupDisallowedSTD3CodePointMapping0(value)), errors, index + indexIncrement)
              }
          case -11 => // DISALLOWED_STD3_MAPPED_MULTI
            if (useStd3ASCIIRules) {
              loop(acc.appendCodePoint(ReplacementCharacter), MappingError(index, "Disallowed code point in input.", CodePoint.unsafeFromInt(value)) :: errors, index + indexIncrement)
            } else {
              loop(unsafeLookupDisallowedSTD3CodePointMultiMapping0(value).foldLeft(acc){
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

  def mapCodePoint(codePoint: CodePoint): CodePointStatus = {
    import CodePointStatus._

    (unsafeMapIntCodePointSentinel0(codePoint.value): @switch) match {
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
        Mapped.of(unsafeLookupCodePointMultiMapping0(codePoint.value).map(CodePoint.unsafeFromInt))
      case -6 => // DEVIATION
        Deviation.one(CodePoint.unsafeFromInt(unsafeLookupDeviationCodePointMapping0(codePoint.value)))
      case -7 => // DEVIATION_MULTI
        Deviation.of(unsafeLookupDeviationCodePointMultiMapping0(codePoint.value).map(CodePoint.unsafeFromInt).toList)
      case -8 => // DISALLOWED
        Disallowed.instance
      case -9 => // DISALLOWED_STD3_VALID
        Disallowed_STD3_Valid.instance
      case -10 => // DISALLOWED_STD3_MAPPED
        Disallowed_STD3_Mapped.one(CodePoint.unsafeFromInt(unsafeLookupDisallowedSTD3CodePointMapping0(codePoint.value)))
      case -11 => // DISALLOWED_STD3_MAPPED_MULTI
        Disallowed_STD3_Mapped.of(unsafeLookupDisallowedSTD3CodePointMultiMapping0(codePoint.value).map(CodePoint.unsafeFromInt))
      case -12 => // DEVIATION_IGNORED
        Deviation.ignored
      case otherwise =>
        // Mapped. There is no sentinel value for mapped, because in this case
        // a code point maps to a single new code point, we just return the
        // mapping directly.
        Mapped.one(CodePoint.unsafeFromInt(otherwise))
    }
  }

  def unsafeMapIntCodePoint(codePoint: Int): Either[String, CodePointStatus] =
    CodePoint.fromInt(codePoint).map(mapCodePoint)

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
