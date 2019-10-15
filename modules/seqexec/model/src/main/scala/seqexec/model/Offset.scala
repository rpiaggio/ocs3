// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.implicits._
import seqexec.model.enum.SystemName

sealed trait OffsetType
object OffsetType {
  sealed trait Telescope extends OffsetType
  sealed trait NodAndShuffle extends OffsetType
  sealed trait NSNodA extends NodAndShuffle
  sealed trait NSNodB extends NodAndShuffle
}

sealed trait OffsetAxis
object OffsetAxis {
  sealed trait P extends OffsetAxis
  sealed trait Q extends OffsetAxis
}

sealed trait OffsetAxisShow[A <: OffsetAxis] {
  val show: String
}
object OffsetAxisShow {
  implicit object OffsetAxisPShow extends OffsetAxisShow[OffsetAxis.P] {
    override val show = "p"
  }

  implicit object OffsetAxisQShow extends OffsetAxisShow[OffsetAxis.Q] {
    override val show = "q"
  }
}

sealed trait OffsetConfigResolver[T <: OffsetType, A <: OffsetAxis] {
  val systemName: SystemName
  val configItem: String
}
object OffsetConfigResolver {
  sealed trait TelescopeOffsetConfigResolver[A <: OffsetAxis]
    extends OffsetConfigResolver[OffsetType.Telescope, A] {
    val systemName = SystemName.Telescope
  }
  implicit object TelescopeOffsetConfigResolverP
    extends TelescopeOffsetConfigResolver[OffsetAxis.P] {
    val configItem = "p"
  }
  implicit object TelescopeOffsetConfigResolverQ
    extends TelescopeOffsetConfigResolver[OffsetAxis.Q] {
    val configItem = "q"
  }

  sealed trait NSOffsetConfigResolver[T <: OffsetType.NodAndShuffle, A <: OffsetAxis]
    extends OffsetConfigResolver[T, A] {
    val systemName = SystemName.Instrument
  }
  sealed trait NSOffsetConfigResolverA[A <: OffsetAxis]
    extends NSOffsetConfigResolver[OffsetType.NSNodA, A]
  implicit object NSOffsetConfigResolverAP
    extends NSOffsetConfigResolverA[OffsetAxis.P] {
    val configItem = "nsBeamA-p"
  }
  implicit object NSOffsetConfigResolverAQ
    extends NSOffsetConfigResolverA[OffsetAxis.Q] {
    val configItem = "nsBeamA-q"
  }
  sealed trait NSOffsetConfigResolverB[A <: OffsetAxis]
    extends NSOffsetConfigResolver[OffsetType.NSNodB, A]
  implicit object NSOffsetConfigResolverBP
    extends NSOffsetConfigResolverB[OffsetAxis.P] {
    val configItem = "nsBeamB-p"
  }
  implicit object NSOffsetConfigResolverBQ
    extends NSOffsetConfigResolverB[OffsetAxis.Q] {
    val configItem = "nsBeamB-q"
  }
}

case class Offset[T <: OffsetType, A <: OffsetAxis](value: Double) extends AnyVal
case object Offset {
  def Zero[T <: OffsetType, A <: OffsetAxis]: Offset[T, A] =
    Offset[T, A](0.0)

  implicit def equal[T <: OffsetType, A <: OffsetAxis]: Eq[Offset[T, A]] =
    Eq.by(_.value)
}
