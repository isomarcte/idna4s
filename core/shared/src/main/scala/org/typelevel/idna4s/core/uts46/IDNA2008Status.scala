package org.typelevel.idna4s.core.uts46

sealed abstract class IDNA2008Status extends Serializable

object IDNA2008Status {
  case object NV8 extends IDNA2008Status
  case object XV8 extends IDNA2008Status
}
