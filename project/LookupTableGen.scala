package org.typelevel.idna4s.build

import cats._
import cats.data._
import cats.syntax.all._
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.collection.immutable.SortedMap

object LookupTableGen {

  // Why is this not in cats?
  private implicit def orderingFromOrder[A](implicit A: Order[A]): Ordering[A] =
    A.toOrdering

  def printAsHex(i: Int): String =
    String.format("0x%04X", i.asInstanceOf[Object]) // Don't ask...

  final case class CasePattern(lower: Int, upper: Int) {
    assert(lower <= upper)

    def inputCount: Int = upper - lower + 1

    def flatten: SortedSet[CasePattern] =
      Range(lower, upper).inclusive.foldLeft(SortedSet.empty[CasePattern]){
        case (acc, value) =>
          acc + CasePattern(value, value)
      }

    def asPatternString(asHex: Boolean): String =
      if (lower == upper) {
        if (asHex) {
          s"case ${printAsHex(lower)}"
        } else {
          s"case $lower"
        }
      } else {
        if (asHex) {
          s"case i if i >= ${printAsHex(lower)} && i <= ${printAsHex(upper)}"
        } else {
          s"case i if i >= $lower && i <= $upper"
        }
      }
  }

  object CasePattern {
    def fromHexStrings(lower: String, upper: String): Either[String, CasePattern] =
      Either.catchNonFatal{
        CasePattern(Integer.parseInt(lower, 16), Integer.parseInt(upper, 16))
      }.leftMap(_.getLocalizedMessage)

    def fromHexString(value: String): Either[String, CasePattern] =
      fromHexStrings(value, value)

    implicit val orderAndHashForCasePattern: Hash[CasePattern] with Order[CasePattern] =
      new Hash[CasePattern] with Order[CasePattern] {
        override final def hash(x: CasePattern): Int = x.hashCode

        override final def compare(x: CasePattern, y: CasePattern): Int =
          x.lower.compare(y.lower) match {
            case 0 =>
              x.upper.compare(y.upper)
            case otherwise =>
              otherwise
          }
      }

    implicit def orderingInstance: Ordering[CasePattern] =
      orderAndHashForCasePattern.toOrdering
  }

  final case class Config(
    flattenRangeThreshold: Int,
    maxCasesPerMethod: Int,
    asHex: Boolean,
    methodBaseName: String,
    argumentName: String,
    finalCase: String,
    startingIndentLevel: Int,
    indentIncrement: Int,
    returnType: String
  )

  object Config {
    def default(
      methodBaseName: String
    ): Config = Config(50, 3500, true, methodBaseName, "value", s"""case otherwise => throw new RuntimeException(s"Invalid input: $$otherwise")${"\n"}""", 1, 2, "Int")

    def defaultList(
      methodBaseName: String
    ): Config = default(methodBaseName).copy(returnType = "NonEmptyList[Int]")
  }

  private def emitIndent(startingIndentLevel: Int, indentIncrement: Int)(level: Int, sb: StringBuilder): StringBuilder =
    sb.append(List.fill((startingIndentLevel + level) * indentIncrement)(' ').mkString)

  private def flattenCases(flattenRangeThreshold: Int, cases: SortedMap[CasePattern, String]): SortedMap[CasePattern, String] =
    cases.flatMap{
      case (pattern, result) =>
        if (pattern.inputCount < flattenRangeThreshold) {
          pattern.flatten.foldLeft(SortedMap.empty[CasePattern, String]){
            case (acc, pattern) =>
              acc + (pattern -> result)
          }
        } else {
          SortedMap(pattern -> result)
        }
    }

  private def partitionIntoMethods(maxCasesPerMethod: Int, cases: SortedMap[CasePattern, String]): NonEmptySet[SortedMap[CasePattern, String]] = {

    def step(value: SortedMap[CasePattern, String]): (NonEmptySet[SortedMap[CasePattern, String]], SortedMap[CasePattern, String]) =
      value.splitAt(maxCasesPerMethod) match {
        case (methodSet, rest) =>
          (NonEmptySet.one(methodSet), rest)
      }

    @tailrec
    def loop(acc: NonEmptySet[SortedMap[CasePattern, String]], value: SortedMap[CasePattern, String]): NonEmptySet[SortedMap[CasePattern, String]] =
      step(value) match {
        case (value, rest) =>
          acc ++ value match {
            // Intentional shadow
            case acc =>
              if (rest.isEmpty) {
                acc
              } else {
                loop(acc, rest)
              }
          }
      }

    step(cases) match {
      case (acc, rest) =>
        if (rest.isEmpty) {
          acc
        } else {
          loop(acc, rest)
        }
    }
  }

  private def partitionBetweenSingleAndMultiCasePatterns(cases: SortedMap[CasePattern, String]): (SortedMap[CasePattern, String], SortedMap[CasePattern, String]) =
    cases.partition{
      case (pattern, _) =>
        pattern.inputCount < 2
    }

  private def emitMethod(methodBaseName: String, methodSuffixBase: String, modifiers: SortedSet[String], argumentName: String, finalCase: String, asHex: Boolean, returnType: String, indent: (Int, StringBuilder) => StringBuilder)(sb: StringBuilder, methodCases: SortedMap[CasePattern, String]): StringBuilder =
    ((indent(1, indent(0, sb).append(
      s"""${modifiers.mkString(" ")} def ${methodBaseName}${methodSuffixBase}(${argumentName}: Int): ${returnType} =${"\n"}""")
    ).append(s"(${argumentName}: @switch) match {\n") match {
      // Intentional shadow
      case sb =>
        methodCases.foldLeft(sb){
          case (acc, (pattern, result)) =>
            indent(2, sb).append(
              s"${pattern.asPatternString(asHex)} => ${result}"
            )
        }
    }) match {
      // Intentional shadow
      case sb =>
        indent(2, sb).append(finalCase)
    }) match {
      // Intentional shadow
      case sb =>
        indent(1, sb).append("}\n\n")
    }

  private def emitMethods(methodBaseName: String, methodSuffixBase: String, initModifiers: SortedSet[String], otherModifiers: SortedSet[String], argumentName: String, finalCase: String, asHex: Boolean, returnType: String, indent: (Int, StringBuilder) => StringBuilder)(sb: StringBuilder, methods: NonEmptySet[SortedMap[CasePattern, String]]): StringBuilder = {

    @tailrec
    def loop(sb: StringBuilder, methods: NonEmptySet[SortedMap[CasePattern, String]], n: Int): StringBuilder =
      (methods.head, methods.tail) match {
        case (method, rest) =>
          val modifiers: SortedSet[String] =
            if (n < 1) {
              initModifiers
            } else {
              otherModifiers
            }
          NonEmptySet.fromSet(rest) match {
            case Some(rest) =>
              val n1: Int = n + 1
              val finalCase: String = s"case otherwise => ${methodBaseName}${methodSuffixBase}${n1}(otherwise)\n"
              loop(emitMethod(methodBaseName, s"${methodSuffixBase}${n}", modifiers, argumentName, finalCase, asHex, returnType, indent)(sb, method), rest, n1)
            case None =>
              emitMethod(methodBaseName, s"${methodSuffixBase}${n}", modifiers, argumentName, finalCase, asHex, returnType, indent)(sb, method)
          }
      }

    loop(sb, methods, 0)
  }

  private def emitSingleAndMultiCasePatternMethods(methodBaseName: String, argumentName: String, finalCase: String, asHex: Boolean, returnType: String, indent: (Int, StringBuilder) => StringBuilder)(sb: StringBuilder, singleCasePatternMethods: NonEmptySet[SortedMap[CasePattern, String]], multiCasePatternMethods: NonEmptySet[SortedMap[CasePattern, String]]): StringBuilder = {
    val multiSuffix: String = "Range"
    val singleCasePatternMethodsFinalCase: String =
      s"case otherwise => ${methodBaseName}${multiSuffix}0(otherwise)\n"
    val privateFinalModifiers: SortedSet[String] = SortedSet("private", "final")

    emitMethods(methodBaseName, "", SortedSet("override", "protected", "final"), privateFinalModifiers, argumentName, singleCasePatternMethodsFinalCase, asHex, returnType, indent)(sb, singleCasePatternMethods) match {
      // Intentional shadow
      case sb =>
        emitMethods(methodBaseName, multiSuffix, privateFinalModifiers, privateFinalModifiers, argumentName, finalCase, asHex, returnType, indent)(sb, multiCasePatternMethods)
    }
  }

  private def partitionAndEmitMethods(methodBaseName: String, argumentName: String, finalCase: String, asHex: Boolean, returnType: String, flattenRangeThreshold: Int, maxCasesPerMethod: Int, indent: (Int, StringBuilder) => StringBuilder)(sb: StringBuilder, cases: SortedMap[CasePattern, String]): StringBuilder =
    (partitionBetweenSingleAndMultiCasePatterns(flattenCases(flattenRangeThreshold, cases)) match {
      case (single, multi) =>
        (partitionIntoMethods(maxCasesPerMethod, single), partitionIntoMethods(maxCasesPerMethod, multi))
    }) match {
      case (single, multi) =>
        emitSingleAndMultiCasePatternMethods(methodBaseName, argumentName, finalCase, asHex, returnType, indent)(sb, single, multi)
    }

  def generateLookupTableMethods(config: Config, cases: SortedMap[CasePattern, String], sb: StringBuilder): StringBuilder =
    partitionAndEmitMethods(
      methodBaseName = config.methodBaseName,
      argumentName = config.argumentName,
      finalCase = config.finalCase,
      asHex = config.asHex,
      returnType = config.returnType,
      flattenRangeThreshold = config.flattenRangeThreshold,
      maxCasesPerMethod = config.maxCasesPerMethod,
      indent = emitIndent(config.startingIndentLevel, config.indentIncrement)
    )(sb, cases)

  def generateLookupTableMethodsString(config: Config, cases: SortedMap[CasePattern, String]): String =
    generateLookupTableMethods(config, cases, new StringBuilder(cases.size * 3)).toString
}
