// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.ActionHandler
import diode.ActionResult
import diode.Effect
import diode.ModelRW
import diode.NoAction
import seqexec.model.events._
import seqexec.web.client.model.Pages._
import seqexec.web.client.ModelOps._
import seqexec.web.client.actions._
import seqexec.web.client.services.SeqexecWebClient
import seqexec.web.client.circuit.SODLocationFocus
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
  * Handles updates to the selected sequences set
  */
class LoadedSequencesHandler[M](modelRW: ModelRW[M, SODLocationFocus])
    extends ActionHandler(modelRW)
    with Handlers[M, SODLocationFocus] {
  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case ServerMessage(LoadSequenceUpdated(i, sid, view, cid)) =>
      // Update selected and the page
      val upSelected = if (value.clientId.exists(_ === cid)) {
        // if I requested the load also focus on it
        SODLocationFocus.sod.modify(
          _.updateFromQueue(view)
            .loadingComplete(sid)
            .unsetPreviewOn(sid)
            .focusOnSequence(i, sid))
      } else {
        SODLocationFocus.sod.modify(
          _.updateFromQueue(view).loadingComplete(sid).unsetPreviewOn(sid))
      }
      val nextStepToRun =
        view.sessionQueue.find(_.id === sid).foldMap(_.nextStepToRun)
      val upLocation = SODLocationFocus.location.set(
        SequencePage(i, sid, nextStepToRun.foldMap(StepIdDisplayed.apply)))
      updatedL(upSelected >>> upLocation)

    case ServerMessage(s: SeqexecModelUpdate) =>
      updated(SODLocationFocus.sod.modify(_.updateFromQueue(s.view))(value))

    case LoadSequence(observer, i, id) =>
      val loadSequence = value.clientId
        .map(cid => Effect(SeqexecWebClient.loadSequence(i, id, observer, cid).map(_ => NoAction)))
        .getOrElse(VoidEffect)
      val update = SODLocationFocus.sod.modify(_.markAsLoading(id))
      updatedLE(update, loadSequence)
  }
}