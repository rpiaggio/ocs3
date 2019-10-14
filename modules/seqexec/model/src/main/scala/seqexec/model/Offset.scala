// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.implicits._
import seqexec.model.enum.SystemName

sealed trait OffsetType
object OffsetType {
  sealed trait Telescope extends OffsetType
  sealed trait NSNodA extends OffsetType
  sealed trait NSNodB extends OffsetType
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
  trait TelescopeOffsetConfigResolver[A <: OffsetAxis]
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
}

case class Offset[T <: OffsetType, A <: OffsetAxis](value: Double)
case object Offset {
  def Zero[T <: OffsetType, A <: OffsetAxis]: Offset[T, A] =
    Offset[T, A](0.0)

  implicit def equal[T <: OffsetType, A <: OffsetAxis]: Eq[Offset[T, A]] =
    Eq.by(_.value)
}
