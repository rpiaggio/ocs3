// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.implicits._
import cats.{Eq, Show}
import cats.effect.Sync
import gem.math.Coordinates
import giapi.client.commands.Configuration
import giapi.client.ghost.GhostClient
import seqexec.server.keywords.GdsClient

import scala.concurrent.duration._
import org.log4s._
import seqexec.server.ConfigUtilOps.{ContentError, ExtractFailure}
import seqexec.server.GiapiInstrumentController
import seqexec.server.ghost.GhostController.GhostConfig

final case class GhostController[F[_]: Sync](override val client: GhostClient[F],
                                             override val gdsClient: GdsClient)
  extends GiapiInstrumentController[F, GhostConfig, GhostClient[F]] {

  import GhostController._
  val Log: Logger = getLogger

  override val name = "GHOST"

  override def configuration(config: GhostConfig): Configuration = config.configuration
}

object GhostController {
  private def ifuConfig(ifuNum: IFUNum,
                        ifuTargetType: IFUTargetType,
                        coordinates: Coordinates,
                        bundleConfig: BundleConfig): Configuration = {
    def cfg[P: Show](paramName: String, paramVal: P) =
      Configuration.single(s"${ifuNum.show}.$paramName", paramVal)
      cfg("target", ifuTargetType) |+|
      cfg("type", DemandType.DemandRADec.demandType) |+|
      cfg("ra", coordinates.ra.toAngle.toDoubleDegrees) |+|
      cfg("dec", coordinates.dec.toAngle.toDoubleDegrees) |+|
      cfg("bundle", bundleConfig.show)
  }

  private def ifuPark(ifuNum: IFUNum): Configuration = {
    def cfg[P: Show](paramName: String, paramVal: P) =
      Configuration.single(s"${ifuNum.ifuStr}.$paramName", paramVal)
      cfg("target", IFUTargetType.NoTarget.targetType) |+|
      cfg("type", DemandType.DemandPark.demandType)
  }

  sealed abstract class BundleConfig(val configName: String) {
    def determineType(t: IFUTargetType): BundleConfig = t match {
      case IFUTargetType.SkyPosition => BundleConfig.Sky
      case _                         => this
    }
  }

  object BundleConfig {
    case object Standard extends BundleConfig(configName = "IFU_LORES")
    case object HighRes  extends BundleConfig(configName = "IFU_HIRES")
    case object Sky      extends BundleConfig(configName = "IFU_SKY")

    implicit val showBundleConfig: Show[BundleConfig] = Show.show(_.configName)
  }

  sealed abstract class IFUNum(val ifuNum: Int) {
    val ifuStr: String = s"ghost:cc:cu:ifu$ifuNum"
  }
  object IFUNum {
    case object IFU1 extends IFUNum(ifuNum = 1)
    case object IFU2 extends IFUNum(ifuNum = 2)

    implicit val showIfuNum: Show[IFUNum] = Show.show(_.ifuStr)
  }

  sealed abstract class IFUTargetType(val targetType: String)
  object IFUTargetType {
    case object NoTarget extends IFUTargetType(targetType = "IFU_TARGET_NONE")
    case object SkyPosition extends IFUTargetType(targetType = "IFU_TARGET_SKY")
    final case class Target(name: String) extends IFUTargetType(targetType = "IFU_TARGET_OBJECT")

    def determineType(name: Option[String]): IFUTargetType = name match {
      case None        => NoTarget
      case Some("Sky") => SkyPosition
      case Some(x)     => Target(x)
    }

    implicit val IFUTargetTypeShow: Show[IFUTargetType] = Show.show(_.targetType)
  }

  sealed trait DemandType {
    def demandType: String
  }
  object DemandType {
    case object DemandRADec extends DemandType {
      val demandType = "IFU_DEMAND_RADEC"
    }
    case object DemandPark extends DemandType {
      val demandType = "IFU_DEMAND_PARK"
    }
  }

  // GHOST has a number of different possible configuration modes: we add types for them here.
  sealed trait GhostConfig {
    def baseCoords: Option[Coordinates]
    def expTime: Duration

    def ifu1TargetType: IFUTargetType
    def ifu2TargetType: IFUTargetType
    def ifu1BundleType: BundleConfig
    def ifu1Coordinates: Coordinates
    def ifu2BundleType: Option[BundleConfig]

    def ifu1Config: Configuration =
      ifuConfig(IFUNum.IFU1, ifu1TargetType, ifu1Coordinates, ifu1BundleType)
    def ifu2Config: Configuration

    def configuration: Configuration = ifu1Config |+| ifu2Config
  }

  sealed trait StandardResolutionMode extends GhostConfig {
    import StandardResolutionMode._

    def ifu1Coordinates: Coordinates

    override def ifu1TargetType: IFUTargetType = this match {
      case s: SingleTarget   => IFUTargetType.Target(s.ifu1TargetName)
      case d: DualTarget     => IFUTargetType.Target(d.ifu1TargetName)
      case ts: TargetPlusSky => IFUTargetType.Target(ts.ifu1TargetName)
      case _: SkyPlusTarget  => IFUTargetType.SkyPosition
    }

    override def ifu2TargetType: IFUTargetType = this match {
      case _: SingleTarget   => IFUTargetType.NoTarget
      case d: DualTarget     => IFUTargetType.Target(d.ifu2TargetName)
      case st: SkyPlusTarget => IFUTargetType.Target(st.ifu2TargetName)
      case _: TargetPlusSky  => IFUTargetType.SkyPosition
    }

    override def ifu1BundleType: BundleConfig = this match {
      case _: SingleTarget | _: DualTarget | _: TargetPlusSky => BundleConfig.Standard
      case _: SkyPlusTarget                                   => BundleConfig.Sky
    }

    override def ifu2BundleType: Option[BundleConfig] = this match {
      case _: SingleTarget                  => None
      case _: DualTarget | _: SkyPlusTarget => Some(BundleConfig.Standard)
      case _: TargetPlusSky                 => Some(BundleConfig.Sky)
    }
  }

  private implicit val durationEq: Eq[Duration] = Eq.by(_.toMillis)

  object StandardResolutionMode {
    final case class SingleTarget(override val baseCoords: Option[Coordinates],
                                  override val expTime: Duration,
                                  ifu1TargetName: String,
                                  override val ifu1Coordinates: Coordinates) extends StandardResolutionMode {
      override def ifu2Config: Configuration =
        ifuPark(IFUNum.IFU2)
    }

    implicit val srmSingleTargetEq: Eq[SingleTarget] = Eq.by(x =>
      (x.baseCoords, x.expTime, x.ifu1TargetName, x.ifu1Coordinates))

    final case class DualTarget(override val baseCoords: Option[Coordinates],
                                override val expTime: Duration,
                                ifu1TargetName: String,
                                override val ifu1Coordinates: Coordinates,
                                ifu2TargetName: String,
                                ifu2Coordinates: Coordinates) extends StandardResolutionMode {
      override def ifu2Config: Configuration =
        ifuConfig(IFUNum.IFU2, IFUTargetType.Target(ifu2TargetName), ifu2Coordinates, BundleConfig.Standard)
    }

    implicit val srmDualTargetEq: Eq[DualTarget] = Eq.by(x =>
      (x.baseCoords, x.expTime, x.ifu1TargetName, x.ifu1Coordinates, x.ifu2TargetName, x.ifu2Coordinates)
    )

    final case class TargetPlusSky(override val baseCoords: Option[Coordinates],
                                   override val expTime: Duration,
                                   ifu1TargetName: String,
                                   override val ifu1Coordinates: Coordinates,
                                   ifu2Coordinates: Coordinates) extends StandardResolutionMode {
      override def ifu2Config: Configuration =
        ifuConfig(IFUNum.IFU2, IFUTargetType.SkyPosition, ifu2Coordinates, BundleConfig.Sky)
    }

    implicit val srmTargetPlusSkyEq: Eq[TargetPlusSky] = Eq.by(x =>
      (x.baseCoords, x.expTime, x.ifu1TargetName, x.ifu1Coordinates, x.ifu2Coordinates))

    final case class SkyPlusTarget(override val baseCoords: Option[Coordinates],
                                   override val expTime: Duration,
                                   override val ifu1Coordinates: Coordinates,
                                   ifu2TargetName: String,
                                   ifu2Coordinates: Coordinates) extends StandardResolutionMode {
      override def ifu2Config: Configuration =
        ifuConfig(IFUNum.IFU2, IFUTargetType.Target(ifu2TargetName), ifu2Coordinates, BundleConfig.Standard)

    }

    implicit val srmSkyPlusTargetEq: Eq[SkyPlusTarget] = Eq.by(x =>
      (x.baseCoords, x.expTime, x.ifu1Coordinates, x.ifu2TargetName, x.ifu2Coordinates))
  }

  sealed trait HighResolutionMode extends GhostConfig {
    import HighResolutionMode._

    override def ifu1BundleType: BundleConfig = BundleConfig.HighRes

    def ifu1TargetName: String

    override def ifu1TargetType: IFUTargetType = IFUTargetType.Target(ifu1TargetName)

    override def ifu2TargetType: IFUTargetType = this match {
      case _: SingleTarget  => IFUTargetType.NoTarget
      case _: TargetPlusSky => IFUTargetType.SkyPosition
    }

    override def ifu2BundleType: Option[BundleConfig] = this match {
      case _: SingleTarget  => None
      case _: TargetPlusSky => Some(BundleConfig.Sky)
    }
  }

  object HighResolutionMode {
    final case class SingleTarget(override val baseCoords: Option[Coordinates],
                                  override val expTime: Duration,
                                  override val ifu1TargetName: String,
                                  override val ifu1Coordinates: Coordinates) extends HighResolutionMode {
      override def ifu2Config: Configuration =
        ifuPark(IFUNum.IFU2)
    }

    implicit val hrSingleTargetEq: Eq[SingleTarget] = Eq.by(x =>
      (x.baseCoords, x.expTime, x.ifu1TargetName, x.ifu1Coordinates))

    final case class TargetPlusSky(override val baseCoords: Option[Coordinates],
                                   override val expTime: Duration,
                                   override val ifu1TargetName: String,
                                   override val ifu1Coordinates: Coordinates,
                                   ifu2Coordinates: Coordinates) extends HighResolutionMode {
      override def ifu2Config: Configuration =
        ifuConfig(IFUNum.IFU2, IFUTargetType.SkyPosition, ifu2Coordinates, BundleConfig.Sky)
    }

    implicit val hrTargetPlusSkyEq: Eq[TargetPlusSky] = Eq.by(x =>
      (x.baseCoords, x.expTime, x.ifu1TargetName, x.ifu1Coordinates, x.ifu2Coordinates))
  }

  // These are the parameters passed to GHOST from the WDBA.
  // We use them to determine the type of configuration being used by GHOST, and instantiate it.
  object GhostConfig {
    def apply(baseCoords: Option[Coordinates],
              expTime: Duration,
              srifu1Name: Option[String],
              srifu1Coords: Option[Coordinates],
              srifu2Name: Option[String],
              srifu2Coords: Option[Coordinates],
              hrifu1Name: Option[String],
              hrifu1Coords: Option[Coordinates],
              hrifu2Name: Option[String],
              hrifu2Coords: Option[Coordinates]): Either[ExtractFailure, GhostConfig] = {
      import IFUTargetType._

      val sifu1 = determineType(srifu1Name)
      val sifu2 = determineType(srifu2Name)
      val hifu1 = determineType(hrifu1Name)
      val hifu2 = determineType(hrifu2Name)
      val extracted = (sifu1, sifu2, hifu1, hifu2) match {
        case (Target(t), NoTarget, NoTarget, NoTarget) =>
          srifu1Coords.map(StandardResolutionMode.SingleTarget.apply(baseCoords, expTime, t, _))
        case (Target(t1), Target(t2), NoTarget, NoTarget) =>
          (srifu1Coords, srifu2Coords).mapN(StandardResolutionMode.DualTarget.apply(baseCoords, expTime, t1, _, t2, _))
        case (Target(t), SkyPosition, NoTarget, NoTarget) =>
          (srifu1Coords, srifu2Coords).mapN(StandardResolutionMode.TargetPlusSky.apply(baseCoords, expTime, t, _, _))
        case (SkyPosition, Target(t), NoTarget, NoTarget) =>
          (srifu1Coords, srifu2Coords).mapN(StandardResolutionMode.SkyPlusTarget.apply(baseCoords, expTime, _, t, _))
        case (NoTarget, NoTarget, Target(t), NoTarget) =>
          hrifu1Coords.map(HighResolutionMode.SingleTarget.apply(baseCoords, expTime, t, _))
        case (NoTarget, NoTarget, Target(t), SkyPosition) =>
          (hrifu1Coords, hrifu2Coords).mapN(HighResolutionMode.TargetPlusSky.apply(baseCoords, expTime, t, _, _))
        case _ =>
          None
      }
      extracted match {
        case Some(config) => Right(config)
        case None         => Left(ContentError("Response does not constitute a valid GHOST configuration"))
      }
    }

    import GhostController.{StandardResolutionMode => SRM, HighResolutionMode => HRM}
    implicit val eq: Eq[GhostConfig] = Eq.instance { (a, b) =>
      (a: Any, b: Any) match {
        case (a: SRM.SingleTarget,  b: SRM.SingleTarget)  => a === b
        case (a: SRM.DualTarget,    b: SRM.DualTarget)    => a === b
        case (a: SRM.TargetPlusSky, b: SRM.TargetPlusSky) => a === b
        case (a: SRM.SkyPlusTarget, b: SRM.SkyPlusTarget) => a === b
        case (a: HRM.SingleTarget,  b: HRM.SingleTarget)  => a === b
        case (a: HRM.TargetPlusSky, b: HRM.TargetPlusSky) => a === b
        case _ => false
      }
    }

    implicit val show: Show[GhostConfig] = Show.fromToString
  }
}