package org.typelevel.idna4s.core.uts46

trait CodePointMapper extends Serializable {
  def mapCodePoints(useStd3ASCIIRules: Boolean, transitionalProcessing: Boolean)(input: String): Either[String, String]
}
