package org.typelevel.idna4s.core.uts46

sealed abstract class CodePointStatus

object CodePointStatus {
  final case class Valid(idna2008Status: Option[IDNA2008Status]) extends CodePointStatus
  final case class Ignored(mapping: List[Int]) extends CodePointStatus
  final case class Mapped(mapping: List[Int]) extends CodePointStatus
  final case class Deviation(mapping: List[Int]) extends CodePointStatus
  case object Disallowed extends CodePointStatus
  case object Disallowed_STD3_Valid extends CodePointStatus
  final case class Disallowed_STD3_Mapped(mapping: List[Int]) extends CodePointStatus
}
