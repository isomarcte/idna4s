/*
 * Copyright (c) 2022 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package org.typelevel.idna4s.core

import cats.kernel._
import cats.Show
import cats.syntax.all._

/**
 * A newtype for representing a Unicode code point.
 *
 * Unicode code points are a integral values from [0, 0x10FFF].
 *
 * This type is not (and should not) generally in critical path performance sensitive code. It
 * is mostly useful for generating error information, testing, and for some performance
 * insensitive methods which operate only a single code point. In particular for errors messages
 * where we generally should not render arbitrary code points as they may influence the output
 * text in ways we do not anticipate, such as bidi scripts or where the code point in isolation
 * is not able to be rendered at all.
 *
 * @see
 *   [[https://www.unicode.org/reports/tr36/ Unicode Security Considerations]]
 */
sealed abstract class CodePoint extends Serializable {

  /**
   * The integral value of the code point.
   */
  def value: Int

  // final

  /**
   * The number of UTF-16 bytes needed to represent this code point. UTF-16 encodes code points
   * >= 0x10000 as surrogate pairs, requiring two bytes to represent.
   */
  final def charCount: Int =
    if (value >= 0x10000) 2 else 1

  /**
   * Convert this code point to a list of utf-16 bytes (char values), either size 1 or 2
   * depending on if this code point is represents a surrogate pair in UTF-16.
   */
  final def toChars: List[Char] =
    Character.toChars(value).toList

  final override def toString: String =
    CodePoint
      .nameForCodePoint(this)
      .fold(
        s"CodePoint(value = ${value}, hexValue = ${CodePoint.intToHex(value)}, charCount = ${charCount})"
      )(name =>
        s"CodePoint(value = ${value}, hexValue = ${CodePoint.intToHex(
          value)}, name = ${name}, charCount = ${charCount})")
}

// CodePointPlatform provides a nameForCodePoint method. This uses the
// Character.getName method on the JVM, but that method is not implemented on
// Native and ScalaJS. It's _really_ nice to have, so I added platform code
// for it so we can at least have it on the JVM.

object CodePoint extends CodePointPlatform {
  final private[this] case class CodePointImpl(override val value: Int) extends CodePoint

  private def intToHex(value: Int): String =
    String.format("0x%04X", Integer.valueOf(value))

  val MinValue: CodePoint = CodePointImpl(0)
  val MaxValue: CodePoint = CodePointImpl(Character.MAX_CODE_POINT)

  /**
   * Attempt to create a [[CodePoint]] from an int, throwing an exception if the int is not a
   * valid Unicode code point.
   */
  def unsafeFromInt(value: Int): CodePoint =
    if (value < 0 || value > Character.MAX_CODE_POINT) {
      throw new IllegalArgumentException(
        s"Given integral value is not a valid Unicode code point: ${intToHex(value)}")
    } else {
      CodePointImpl(value)
    }

  /**
   * Attempt to create a [[CodePoint]] from an int, yield an error if the int is not a valid
   * Unicode code point.
   */
  def fromInt(value: Int): Either[String, CodePoint] =
    Either.catchNonFatal(unsafeFromInt(value)).leftMap(_.getLocalizedMessage)

  /**
   * Attempt to create a [[CodePoint]] from a char value, throwing an exception if the char is
   * not a valid Unicode code point.
   *
   * Only char values which are not part of UTF-16 surrogate pairs are valid.
   */
  def unsafeFromChar(value: Char): CodePoint =
    if (value >= Character.MIN_SURROGATE) {
      throw new IllegalArgumentException(
        s"Char values which are part of a UTF-16 surrogate pair do not represent complete Unicode code points: ${intToHex(
          value.toInt)}")
    } else {
      CodePointImpl(value.toInt)
    }

  /**
   * Attempt to create a [[CodePoint]] from a char value, yielding an error if the char is not a
   * valid Unicode code point.
   *
   * Only char values which are not part of UTF-16 surrogate pairs are valid.
   */
  def fromChar(value: Char): Either[String, CodePoint] =
    Either.catchNonFatal(unsafeFromChar(value)).leftMap(_.getLocalizedMessage)

  /**
   * Attempt to parse a `String` as a Unicode code point, throwing an exception if the `String`
   * is not a valid Unicode code point.
   *
   * Base 10 and base 16 numbers are valid. Base 16 values must be prefixed with "0x" or "0X".
   */
  def unsafeFromString(value: String): CodePoint =
    if (value.trim.toLowerCase.startsWith("0x")) {
      unsafeFromInt(Integer.parseInt(value.trim.drop(2), 16))
    } else {
      unsafeFromInt(value.toInt)
    }

  /**
   * Attempt to parse a `String` as a Unicode code point, yielding an error if the `String` is
   * not a valid Unicode code point.
   *
   * Base 10 and base 16 numbers are valid. Base 16 values must be prefixed with "0x" or "0X".
   */
  def fromString(value: String): Either[String, CodePoint] =
    Either.catchNonFatal(unsafeFromString(value)).leftMap(_.getLocalizedMessage)

  implicit val hashAndOrderForCodePoint: Hash[CodePoint] with Order[CodePoint] =
    new Hash[CodePoint] with Order[CodePoint] {
      override def hash(x: CodePoint): Int = x.hashCode

      override def compare(x: CodePoint, y: CodePoint): Int =
        x.value.compare(y.value)
    }

  implicit def ordering: Ordering[CodePoint] =
    hashAndOrderForCodePoint.toOrdering

  implicit val showForCodePoint: Show[CodePoint] =
    Show.fromToString

  implicit val lowerBoundForCodePoint: LowerBounded[CodePoint] =
    new LowerBounded[CodePoint] {
      override def partialOrder: PartialOrder[CodePoint] = hashAndOrderForCodePoint

      override def minBound: CodePoint = CodePoint.MinValue
    }

  implicit val upperBoundForCodePoint: UpperBounded[CodePoint] =
    new UpperBounded[CodePoint] {
      override def partialOrder: PartialOrder[CodePoint] = hashAndOrderForCodePoint

      override def maxBound: CodePoint = CodePoint.MaxValue
    }
}