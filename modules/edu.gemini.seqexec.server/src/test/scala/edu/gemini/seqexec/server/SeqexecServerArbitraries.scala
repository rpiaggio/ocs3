// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import edu.gemini.seqexec.server.flamingos2.Flamingos2Controller
import edu.gemini.seqexec.server.gcal.GcalController
import edu.gemini.seqexec.server.gcal.GcalController._
import edu.gemini.seqexec.server.tcs.{BinaryOnOff, BinaryYesNo, TcsController, TcsControllerEpics}
import edu.gemini.spModel.gemini.flamingos2.Flamingos2
import edu.gemini.spModel.gemini.gnirs.GNIRSParams
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Cogen, Gen}
import squants.space.LengthConversions._

object SeqexecServerArbitraries {

  implicit val observeCommandArb: Arbitrary[ObserveCommand.Result] = Arbitrary(Gen.oneOf(ObserveCommand.Success, ObserveCommand.Paused, ObserveCommand.Aborted, ObserveCommand.Stopped))
  implicit val observeCommandCogen: Cogen[ObserveCommand.Result] =
    Cogen[String].contramap(_.productPrefix)
  implicit val binaryYNArb: Arbitrary[BinaryYesNo] = Arbitrary(Gen.oneOf(BinaryYesNo.Yes, BinaryYesNo.No))
  implicit val binaryYNCommandCogen: Cogen[BinaryYesNo] =
    Cogen[String].contramap(_.name)
  implicit val binaryOOArb: Arbitrary[BinaryOnOff] = Arbitrary(Gen.oneOf(BinaryOnOff.Off, BinaryOnOff.On))
  implicit val binaryOOCommandCogen: Cogen[BinaryOnOff] =
    Cogen[String].contramap(_.name)
  implicit val tcsBeamArb: Arbitrary[TcsController.Beam] = Arbitrary(Gen.oneOf(TcsController.Beam.A, TcsController.Beam.B, TcsController.Beam.C))
  implicit val tcsBeamCogen: Cogen[TcsController.Beam] =
    Cogen[String].contramap(_.productPrefix)
  implicit val tcsNodChopArb: Arbitrary[TcsController.NodChop] = Arbitrary {
    for {
      n <- arbitrary[TcsController.Beam]
      c <- arbitrary[TcsController.Beam]
    } yield TcsController.NodChop(n, c)
  }
  implicit val tcsNodChopCogen: Cogen[TcsController.NodChop] =
    Cogen[(TcsController.Beam, TcsController.Beam)].contramap(x => (x.nod, x.chop))

  private val lengthMMGen = Gen.posNum[Double].map(_.millimeters)
  implicit val offsetXArb: Arbitrary[TcsController.OffsetX] = Arbitrary(lengthMMGen.map(TcsController.OffsetX.apply))
  implicit val offsetXCogen: Cogen[TcsController.OffsetX] =
    Cogen[Double].contramap(_.self.value)
  implicit val offsetYArb: Arbitrary[TcsController.OffsetY] = Arbitrary(lengthMMGen.map(TcsController.OffsetY.apply))
  implicit val offsetYCogen: Cogen[TcsController.OffsetY] =
    Cogen[Double].contramap(_.self.value)
  implicit val fpoArb: Arbitrary[TcsController.FocalPlaneOffset] = Arbitrary {
    for {
      x <- arbitrary[TcsController.OffsetX]
      y <- arbitrary[TcsController.OffsetY]
    } yield TcsController.FocalPlaneOffset(x, y)
  }
  implicit val fpoCogen: Cogen[TcsController.FocalPlaneOffset] =
    Cogen[(TcsController.OffsetX, TcsController.OffsetY)].contramap(x => (x.x, x.y))
  implicit val offsetAArb: Arbitrary[TcsController.OffsetA] = Arbitrary(arbitrary[TcsController.FocalPlaneOffset].map(TcsController.OffsetA.apply))
  implicit val offsetACogen: Cogen[TcsController.OffsetA] =
    Cogen[TcsController.FocalPlaneOffset].contramap(_.self)
  implicit val offsetBArb: Arbitrary[TcsController.OffsetB] = Arbitrary(arbitrary[TcsController.FocalPlaneOffset].map(TcsController.OffsetB.apply))
  implicit val offsetBCogen: Cogen[TcsController.OffsetB] =
    Cogen[TcsController.FocalPlaneOffset].contramap(_.self)
  implicit val offsetCArb: Arbitrary[TcsController.OffsetC] = Arbitrary(arbitrary[TcsController.FocalPlaneOffset].map(TcsController.OffsetC.apply))
  implicit val offsetCCogen: Cogen[TcsController.OffsetC] =
    Cogen[TcsController.FocalPlaneOffset].contramap(_.self)
  implicit val sfInstNameArb: Arbitrary[TcsControllerEpics.CodexScienceFoldPosition.SFInstName] = Arbitrary(Gen.oneOf(TcsControllerEpics.CodexScienceFoldPosition.instNameMap.values.toSeq))
  implicit val sfInstNameCogen: Cogen[TcsControllerEpics.CodexScienceFoldPosition.SFInstName] =
    Cogen[String].contramap(_.self)

  implicit val f2FPUArb: Arbitrary[Flamingos2.FPUnit] = Arbitrary(Gen.oneOf(Flamingos2.FPUnit.values()))
  implicit val f2FPUCogen: Cogen[Flamingos2.FPUnit] =
    Cogen[String].contramap(_.displayValue())
  implicit val f2CFPUArb: Arbitrary[Flamingos2Controller.FocalPlaneUnit] = Arbitrary(Gen.oneOf(Flamingos2Controller.FocalPlaneUnit.Open, Flamingos2Controller.FocalPlaneUnit.GridSub1Pix,
    Flamingos2Controller.FocalPlaneUnit.Grid2Pix, Flamingos2Controller.FocalPlaneUnit.Slit1Pix, Flamingos2Controller.FocalPlaneUnit.Slit2Pix,
    Flamingos2Controller.FocalPlaneUnit.Slit3Pix, Flamingos2Controller.FocalPlaneUnit.Slit4Pix, Flamingos2Controller.FocalPlaneUnit.Slit6Pix, Flamingos2Controller.FocalPlaneUnit.Slit8Pix))
  implicit val f2CFPUCogen: Cogen[Flamingos2Controller.FocalPlaneUnit] =
    Cogen[String].contramap(_.productPrefix)

  implicit val gnirsAmArb: Arbitrary[GNIRSParams.AcquisitionMirror] = Arbitrary(Gen.oneOf(GNIRSParams.AcquisitionMirror.values()))
  implicit val gnirsAmCogen: Cogen[GNIRSParams.AcquisitionMirror] =
    Cogen[String].contramap(_.displayValue())
  implicit val gnirsWpArb: Arbitrary[GNIRSParams.WollastonPrism] = Arbitrary(Gen.oneOf(GNIRSParams.WollastonPrism.values()))
  implicit val gnirsWpCogen: Cogen[GNIRSParams.WollastonPrism] =
    Cogen[String].contramap(_.displayValue())
  implicit val gnirsSwArb: Arbitrary[GNIRSParams.SlitWidth] = Arbitrary(Gen.oneOf(GNIRSParams.SlitWidth.values()))
  implicit val gnirsSwCogen: Cogen[GNIRSParams.SlitWidth] =
    Cogen[String].contramap(_.displayValue())
  implicit val gnirsCdArb: Arbitrary[GNIRSParams.CrossDispersed] = Arbitrary(Gen.oneOf(GNIRSParams.CrossDispersed.values()))
  implicit val gnirsCdCogen: Cogen[GNIRSParams.CrossDispersed] =
    Cogen[String].contramap(_.displayValue())
  implicit val gnirsDeArb: Arbitrary[GNIRSParams.Decker] = Arbitrary(Gen.oneOf(GNIRSParams.Decker.values()))
  implicit val gnirsDeCogen: Cogen[GNIRSParams.Decker] =
    Cogen[String].contramap(_.displayValue())
  implicit val gnirsCaArb: Arbitrary[GNIRSParams.Camera] = Arbitrary(Gen.oneOf(GNIRSParams.Camera.values()))
  implicit val gnirsCaCogen: Cogen[GNIRSParams.Camera] =
    Cogen[String].contramap(_.displayValue())

  implicit val gcalLampArb: Arbitrary[GcalController.LampState] = Arbitrary(Gen.oneOf(GcalController.LampState.On, GcalController.LampState.Off))
  implicit val gcalLampCogen: Cogen[GcalController.LampState] =
    Cogen[String].contramap(_.productPrefix)
  implicit val gcalArLampArb: Arbitrary[GcalController.ArLampState] = Arbitrary(arbitrary[GcalController.LampState].map(ArLampState.apply))
  implicit val gcalArLampCogen: Cogen[GcalController.ArLampState] =
    Cogen[GcalController.LampState].contramap(_.self)
  implicit val gcalCuArLampArb: Arbitrary[GcalController.CuArLampState] = Arbitrary(arbitrary[GcalController.LampState].map(CuArLampState.apply))
  implicit val gcalCuArLampCogen: Cogen[GcalController.CuArLampState] =
    Cogen[GcalController.LampState].contramap(_.self)
  implicit val gcalQhLampArb: Arbitrary[GcalController.QHLampState] = Arbitrary(arbitrary[GcalController.LampState].map(QHLampState.apply))
  implicit val gcalQhLampCogen: Cogen[GcalController.QHLampState] =
    Cogen[GcalController.LampState].contramap(_.self)
  implicit val gcalThArLampArb: Arbitrary[GcalController.ThArLampState] = Arbitrary(arbitrary[GcalController.LampState].map(ThArLampState.apply))
  implicit val gcalThArLampCogen: Cogen[GcalController.ThArLampState] =
    Cogen[GcalController.LampState].contramap(_.self)
  implicit val gcalXeLampArb: Arbitrary[GcalController.XeLampState] = Arbitrary(arbitrary[GcalController.LampState].map(XeLampState.apply))
  implicit val gcalXeLampCogen: Cogen[GcalController.XeLampState] =
    Cogen[GcalController.LampState].contramap(_.self)
  implicit val gcalIrLampArb: Arbitrary[GcalController.IrLampState] = Arbitrary(arbitrary[GcalController.LampState].map(IrLampState.apply))
  implicit val gcalIrLampCogen: Cogen[GcalController.IrLampState] =
    Cogen[GcalController.LampState].contramap(_.self)
}