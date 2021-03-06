// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.extra.TimerSupport
import react.common._
import react.semanticui.modules.progress.Progress
import react.semanticui.colors._
import seqexec.model.ObserveStage
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.reusability._

final case class SmoothObservationProgressBar(
  fileId:   String,
  total:    Int,
  value:    Int,
  stopping: Boolean,
  paused:   Boolean,
  stage:    ObserveStage
) extends SmoothProgressBarProps {
  @inline def render: VdomElement = SmoothObservationProgressBar.component(this)

  override val maxValue = total
}

object SmoothObservationProgressBar
    extends SmoothProgressBar[SmoothObservationProgressBar]
    with ProgressLabel {
  type Props = SmoothObservationProgressBar

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  protected val component = ScalaComponent
    .builder[Props]("SmoothProgressBar")
    .initialStateFromProps(State.fromProps)
    .backend(x => new Backend(x))
    .render_PS { (p, s) =>
      val remainingMillis = s.maxValue - s.value

      Progress(
        total = p.total,
        value = s.value,
        color = Blue,
        clazz = SeqexecStyles.observationProgressBar
      )(label(p.fileId, remainingMillis.some, p.stopping, p.paused, p.stage))
    }
    .componentDidMount(_.backend.setupTimer)
    .componentWillReceiveProps(x => x.backend.newStateFromProps(x.currentProps, x.nextProps))
    .configure(TimerSupport.install)
    .configure(Reusability.shouldComponentUpdate)
    .build
}

