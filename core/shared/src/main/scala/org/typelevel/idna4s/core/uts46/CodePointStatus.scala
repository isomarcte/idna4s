package org.typelevel.idna4s.core.uts46

import cats.data._
import org.typelevel.idna4s.core._

sealed abstract class CodePointStatus extends Serializable

object CodePointStatus {
  sealed abstract class Valid extends CodePointStatus {
    def idna2008Status: Option[IDNA2008Status]

    override final def toString: String = s"Valid(idna2008Status = ${idna2008Status})"
  }

  object Valid {
    private[this] case object Always extends Valid {
      override val idna2008Status: Option[IDNA2008Status] = None
    }

    private[this] case object NV8 extends Valid {
      override val idna2008Status: Option[IDNA2008Status] = Some(IDNA2008Status.NV8)
    }

    private[this] case object XV8 extends Valid {
      override val idna2008Status: Option[IDNA2008Status] = Some(IDNA2008Status.XV8)
    }

    def always: Valid = Always
    def nv8: Valid = NV8
    def xv8: Valid = XV8

    def apply(idna2008Status: Option[IDNA2008Status]): Valid =
      idna2008Status.fold(
        always
      ){
        case IDNA2008Status.NV8 => nv8
        case IDNA2008Status.XV8 => xv8
      }
  }

  sealed abstract class Ignored extends CodePointStatus {
    override final def toString: String = "Ignored"
  }

  object Ignored {
    private[this] final case object IgnoredImpl extends Ignored

    val instance: Ignored = IgnoredImpl
  }

  sealed abstract class Mapped extends CodePointStatus {
    def mapping: NonEmptyList[CodePoint]

    override final def toString: String = s"Mapped(mapping = ${mapping})"
  }

  object Mapped {
    private[this] final case class MappedImpl(override val mapping: NonEmptyList[CodePoint]) extends Mapped

    def of(mapping: NonEmptyList[CodePoint]): Mapped =
      MappedImpl(mapping)

    def one(mapping: CodePoint): Mapped =
      of(NonEmptyList.one(mapping))
  }

  sealed abstract class Deviation extends CodePointStatus {
    def mapping: List[CodePoint]

    override final def toString: String = s"Deviation(mapping = ${mapping})"
  }

  object Deviation {
    private[this] final case class DeviationImpl(override val mapping: List[CodePoint]) extends Deviation

    val ignored: Deviation = DeviationImpl(Nil)

    def of(mapping: List[CodePoint]): Deviation =
      DeviationImpl(mapping)

    def one(mapping: CodePoint): Deviation =
      of(List(mapping))
  }

  sealed abstract class Disallowed extends CodePointStatus {
    override final def toString: String = "Disallowed"
  }

  object Disallowed {
    private[this] case object DisallowedImpl extends Disallowed

    val instance: Disallowed = DisallowedImpl
  }

  sealed abstract class Disallowed_STD3_Valid extends CodePointStatus {
    override final def toString: String = "Disallowed_STD3_Valid"
  }

  object Disallowed_STD3_Valid {
    private[this] case object Disallowed_STD3_ValidImpl extends Disallowed_STD3_Valid

    val instance: Disallowed_STD3_Valid = Disallowed_STD3_ValidImpl
  }

  sealed abstract class Disallowed_STD3_Mapped extends CodePointStatus {
    def mapping: NonEmptyList[CodePoint]

    override final def toString: String = s"Disallowed_STD3_Mapped(mapping = ${mapping})"
  }

  object Disallowed_STD3_Mapped {
    private[this] final case class Disallowed_STD3_MappedImpl(override val mapping: NonEmptyList[CodePoint]) extends Disallowed_STD3_Mapped

    def of(mapping: NonEmptyList[CodePoint]): Disallowed_STD3_Mapped =
      Disallowed_STD3_MappedImpl(mapping)

    def one(mapping: CodePoint): Disallowed_STD3_Mapped =
      of(NonEmptyList.one(mapping))
  }
}
