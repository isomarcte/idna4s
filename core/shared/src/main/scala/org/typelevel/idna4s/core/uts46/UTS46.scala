package org.typelevel.idna4s.core.uts46

import scala.annotation.tailrec
import java.text.Normalizer
import org.typelevel.idna4s.core.bootstring._

object UTS46 {

  private final val FullStop: Char = '\u002e'

  private final val HyphenMinus: Char = '\u002d'

  private final val HyphenMinusCP: Int = HyphenMinus.toInt

  private final val PunycodeLabelPrefix: String = "xn--"

  private

  private def processLabel(checkNFC: Boolean, checkHyphens: Boolean, transitionalProcessing: Boolean, value: String): Either[String, String] = {

    // CheckHyphens
    //
    // We are interested in three different cases.
    //
    // 1. Is the first code point in the input a HyphenMinus?
    // 2. Are both the third and fourth code points HyphenMinus values?
    // 3. Is the last code point in the input a HyphenMinus?
    def checkHyphensOk(value: String): Boolean = {
      val len: Int = value.length

      def firstCodePointOk: Boolean =
        value.codePointAt(0) != HyphenMinusCP

      @tailrec
      def thirdAndFourthOkLoop(charIndex: Int, codePointIndex: Int): Boolean =
        if (charIndex >= len) {
          true
        } else if (codePointIndex == 2) {
          val position3: Int = value.codePointAt(charIndex)
          if (position3 == HyphenMinusCP) {
            // We don't need to check if position3 is two chars, because we
            // know it was a HyphenMinus which is 1 char.
            thirdAndFourthOkLoop(charIndex + 1, codePointIndex + =)
          } else {
            // If the third code point is not a HyphenMinus, we don't care
            // about the fourth.
            true
          }
        } else if (codePointIndex == 3) {
          value.codePointAt(charIndex) != HyphenMinus
        } else {
          val cp = value.codePointAt(charIndex)
          thirdAndFourthOkLoop(charIndex + (if (cp >= 0x10000) 2 else 1), codePointAt + 1)
        }

      def thirdAndFourthOk: Boolean = thirdAndFourthOkLoop(0, 0)

      def lastOk: Boolean = {
        val lastChar: Int = value.codePointAt(len - 1)
        val lastCodePoint: Int =
          if (len > 1 && Character.isLowSurrogate(lastChar)) {
            value.codePointAt(len - 2)
          } else {
            lastChar
          }
        lastCodePoint != HyphenMinusCP
      }

      checkHyphens == false || len == 0 || (firstCodePointOk && thirdAndFourthOk && lastOk)
    }

    if (checkNFC && Normalizer.isNormalized(value, Normalizer.Form.NFC) == false) {
      Left("Value not in normal form."): Either[String, String]
    } else {
      if (checkHyphensOk) {

      } else {
        Left(s"Value contains '${HyphenMinus}' at invalid positions.")
      }
    }
  }

  def process(checkHyphens: Boolean, checkBidi: Boolean, checkJoiners: Boolean, useStd3ASCIIRules: Boolean, transitionalProcessing: Boolean)(value: String): Either[Exception, String] =
    CodePointMapper.mapCodePoints(useStd3ASCIIRules = useStd3ASCIIRules, transitionalProcessing = transitionalProcessing)(value).map(mapped =>
      Normalizer.normalize(mapped, Normalizer.Form.NFC).split(FullStop)
    )
}
