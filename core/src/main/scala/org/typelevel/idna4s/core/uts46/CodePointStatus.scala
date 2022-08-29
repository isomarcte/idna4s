package org.typelevel.idna4s.core.uts46

sealed abstract class CodePointStatus extends Serializable

// object CodePointStatus {
//   sealed abstract class Valid extends CodePointStatus {
//     def idna2008Status: Option[IDNA2008Status]

//     override final def toString: String = s"Valid(idna2008Status = ${idna2008Status})"
//   }

//   object Valid {
//     private[this] case object Empty extends Valid {
//       override val idna2008Status: Option[IDNA2008Status] = None
//     }

//     private[this] case object NV8 extends Valid {
//       override val idna2008Status: Option[IDNA2008Status] = Some(IDNA2008Status.NV8)
//     }

//     private[this] case object XV8 extends Valid {
//       override val idna2008Status: Option[IDNA2008Status] = Some(IDNA2008Status.XV8)
//     }

//     def empty: Valid = Empty
//     def nv8: Valid = NV8
//     def xv8: Valid = XV8

//     def apply(idna2008Status: Option[IDNA2008Status]): Valid =
//       idna2008Status.fold(
//         empty
//       ){
//         case IDNA2008Status.NV8 => nv8
//         case IDNA2008Status.XV8 => xv8
//       }
//   }

//   sealed abstract class Ignored extends CodePointStatus {
//     def mapping: SortedSet[Int]

//     override final def toString: String = s"Ignored(mapping = ${mapping})"
//   }

//   object Ignored {
//     private[this] final case class IgnoredImpl(override val mapping: SortedSet[Int]) extends Ignored

//     val empty: Ignored = unsafeFrom(SortedSet.empty[Int])

//     def unsafeFrom(mapping: SortedSet[Int]): Ignored =
//       IgnoredImpl(mapping)

//     def from(mapping: SortedSet[Int]): Either[String, Ignored] =
//       if(mapping.forall(cp => cp > 0 && cp <= Character.MAX_CODE_POINT)) {
//         Right(unsafeFrom(mapping))
//       } else {
//         Left(s"Mapping contains integral values which are not valid Unicode code points.")
//       }
//   }

//   sealed abstract class Mapped extends CodePointStatus {
//     def mapping: NonEmptySet[Int]

//     override final def toString: String = s"Mapped(mapping = ${mapping})"
//   }

//   object Mapped {
//     private[this] final case class MappedImpl(override val mapping: NonEmptySet[Int]) extends Mapped

//     def unsafeFrom(mapping: NonEmptySet[Int]): Mapped =
//       MappedImpl(mapping)

//     def from(mapping: NonEmptySet[Int]): Either[String, Mapped] =
//       if(mapping.forall(cp => cp > 0 && cp <= Character.MAX_CODE_POINT)) {
//         Right(unsafeFrom(mapping))
//       } else {
//         Left(s"Mapping contains integral values which are not valid Unicode code points.")
//       }
//   }

//   sealed abstract class Deviation extends CodePointStatus {
//     def mapping: SortedSet[Int]

//     override final def toString: String = s"Deviation(mapping = ${mapping})"
//   }

//   object Deviation {
//     private[this] final case class DeviationImpl(override val mapping: SortedSet[Int]) extends Deviation

//     val empty: Deviation = unsafeFrom(SortedSet.empty[Int])

//     def unsafeFrom(mapping: SortedSet[Int]): Deviation =
//       DeviationImpl(mapping)

//     def from(mapping: SortedSet[Int]): Either[String, Deviation] =
//       if(mapping.forall(cp => cp > 0 && cp <= Character.MAX_CODE_POINT)) {
//         Right(unsafeFrom(mapping))
//       } else {
//         Left(s"Mapping contains integral values which are not valid Unicode code points.")
//       }
//   }

//   sealed abstract class Disallowed extends CodePointStatus {
//     override final def toString: String = "Disallowed"
//   }

//   object Disallowed {
//     private[this] case object DisallowedImpl extends Disallowed

//     val instance: Disallowed = DisallowedImpl
//   }

//   sealed abstract class Disallowed_STD3_Valid extends CodePointStatus {
//     override final def toString: String = "Disallowed_STD3_Valid"
//   }

//   object Disallowed_STD3_Valid {
//     private[this] case object Disallowed_STD3_ValidImpl extends Disallowed_STD3_Valid

//     val instance: Disallowed_STD3_Valid = Disallowed_STD3_ValidImpl
//   }

//   sealed abstract class Disallowed_STD3_Mapped extends CodePointStatus {
//     def mapping: NonEmptySet[Int]

//     override final def toString: String = s"Disallowed_STD3_Mapped(mapping = ${mapping})"
//   }

//   object Disallowed_STD3_Mapped {
//     private[this] final case class Disallowed_STD3_MappedImpl(override val mapping: NonEmptySet[Int]) extends Disallowed_STD3_Mapped

//     def unsafeFrom(mapping: NonEmptySet[Int]): Disallowed_STD3_Mapped =
//       Disallowed_STD3_MappedImpl(mapping)

//     def from(mapping: NonEmptySet[Int]): Either[String, Disallowed_STD3_Mapped] =
//       if(mapping.forall(cp => cp > 0 && cp <= Character.MAX_CODE_POINT)) {
//         Right(unsafeFrom(mapping))
//       } else {
//         Left(s"Mapping contains integral values which are not valid Unicode code points.")
//       }
//   }
// }
