// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Reusability
import react.common.implicits._
import seqexec.model.Step
import seqexec.model.OffsetAxis
import seqexec.web.client.model.StepItems._
import seqexec.web.client.model.Formatting._
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.semanticui.elements.icon.Icon.{IconBan, IconCrosshairs}
import seqexec.web.client.semanticui.Size
import seqexec.web.client.reusability._
import web.client.ReactProps

/**
  * Component to display the offsets
  */
final case class OffsetsDisplayCell(
  offsetsDisplay: OffsetsDisplay,
  step: Step
) extends ReactProps {
  @inline def render: VdomElement = OffsetsDisplayCell.component(this)
}

object OffsetsDisplayCell {
  type Props = OffsetsDisplayCell

  implicit val doubleReuse: Reusability[Double] = Reusability.double(0.0001)
  implicit val ofdReuse: Reusability[OffsetsDisplay] = Reusability.derive[OffsetsDisplay]
  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  private val guidingIcon = IconCrosshairs.copyIcon(color = "green".some, size = Size.Large)
  private val noGuidingIcon = IconBan.copyIcon(size = Size.Large)

  protected val component = ScalaComponent.builder[Props]("OffsetsDisplayCell")
    .stateless
    .render_P { p =>
      p.offsetsDisplay match {
        case OffsetsDisplay.DisplayOffsets(offsetWidth) =>
          val offsetP = p.step.offset[OffsetAxis.P]
          val offsetQ = p.step.offset[OffsetAxis.Q]
          val guiding = p.step.guiding

          <.div(
            SeqexecStyles.guidingCell,
            guidingIcon.when(guiding),
            noGuidingIcon.unless(guiding),
            <.div(
              SeqexecStyles.inlineBlock,
              SeqexecStyles.offsetsBlock,
              ^.textAlign := "right",
              <.div(
                <.div(
                  ^.width := pLabelWidth.px,
                  SeqexecStyles.inlineBlock,
                  offsetAxis[OffsetAxis.P]
                ),
                <.div(
                  ^.width := offsetWidth.px,
                  SeqexecStyles.inlineBlock,
                  offsetValueFormat(offsetP)
                )
              ),
              <.div(
                  SeqexecStyles.inlineBlock,
                <.div(
                  ^.width := qLabelWidth.px,
                  SeqexecStyles.inlineBlock,
                  offsetAxis[OffsetAxis.Q]
                ),
                <.div(
                  ^.width := offsetWidth.px,
                  SeqexecStyles.inlineBlock,
                  offsetValueFormat(offsetQ)
                )
              )
            )
          )
        case _ => <.div()
      }
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
