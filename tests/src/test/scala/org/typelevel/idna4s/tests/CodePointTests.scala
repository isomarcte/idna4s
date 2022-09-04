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

package org.typelevel.idna4s.tests

import org.scalacheck._
import org.scalacheck.Prop._
import scala.annotation.tailrec
import cats.kernel.laws.discipline._
import munit._
import org.typelevel.idna4s.core._
import org.typelevel.idna4s.scalacheck.all._

final class CodePointTests extends DisciplineSuite {

  test("CodePoint.fromInt should succeed for all code points") {

    @tailrec
    def loop(i: Int): Unit =
      if (i > Character.MAX_CODE_POINT) {
        ()
      } else {
        assert(CodePoint.fromInt(i).isRight)
        loop(i + 1)
      }

    loop(0)
  }

  test("CodePoint round trip") {

    @tailrec
    def loop(i: Int): Unit =
      if (i > Character.MAX_CODE_POINT) {
        ()
      } else {
        val cp: CodePoint = CodePoint.unsafeFromInt(i)
        assertEquals(cp.value, i)

        cp.toChars match {
          case x :: Nil =>
            assertEquals(CodePoint.unsafeFromInt(x.toInt), cp)
          case x :: y :: Nil =>
            assertEquals(CodePoint.unsafeFromInt(Character.toCodePoint(x, y)), cp)
          case _ =>
            fail(s"CodePoint ${i} did not yield 1 or 2 chars.")
        }

        loop(i + 1)
      }

    loop(0)
  }

  test("CodePoint.fromChar should succeed for non-surrogate chars") {

    @tailrec
    def loop(i: Int): Unit =
      if (i >= Character.MIN_SURROGATE) {
        ()
      } else {
        CodePoint.fromChar(i.toChar) match {
          case Left(e) => fail(e)
          case Right(cp) =>
            assertEquals(cp.value, i)
        }

        loop(i + 1)
      }

    loop(0)

  }

  test("CodePoint.fromString should parse valid code points") {

    @tailrec
    def loop(i: Int): Unit =
      if (i > Character.MAX_CODE_POINT) {
        ()
      } else {
        assert(CodePoint.fromString(i.toString).isRight)
        assert(CodePoint.fromString(String.format("0x%04X", Integer.valueOf(i))).isRight)
        assert(CodePoint.fromString(String.format("0X%04X", Integer.valueOf(i))).isRight)

        loop(i + 1)
      }

    loop(0)
  }

  test("CodePoint.fromString fails for non integral strings") {
    assert(CodePoint.fromString("DERP").isLeft)
  }

  property("CodePoint.fromInt should fail for non code points")(
    forAll(genNonCodePoint)(i => Prop(CodePoint.fromInt(i).isLeft) :| "CodePoint.fromInt fails")
  )

  property("CodePoint.fromChar should fail for surrogate char values")(
    forAll(Gen.choose(Character.MIN_SURROGATE, Char.MaxValue))(c =>
      Prop(CodePoint.fromChar(c).isLeft) :| "CodePoint.fromChar fails")
  )

  // Laws //

  checkAll("Order[CodePoint]", OrderTests[CodePoint].order)
  checkAll("Hash[CodePoint]", HashTests[CodePoint].hash)
  checkAll("LowerBounded[CodePoint]", LowerBoundedTests[CodePoint].lowerBounded)
  checkAll("UpperBounded[CodePoint]", UpperBoundedTests[CodePoint].upperBounded)
}