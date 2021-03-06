// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import cats.implicits._
import diode.react.ReactPot._
import gem.enum.Site
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.React
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.common.implicits._
import react.semanticui.collections.grid._
import react.semanticui.elements.divider.Divider
import react.semanticui.widths._
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.components.tabs.TabsArea
import seqexec.web.client.model.Pages._
import seqexec.web.client.model.WebSocketConnection
import seqexec.web.client.reusability._

final case class AppTitle(site: Site, ws: WebSocketConnection) extends ReactProps {
  @inline def render: VdomElement = AppTitle.component(this)
}

object AppTitle {
  type Props = AppTitle

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  private val component = ScalaComponent
    .builder[Props]("SeqexecTitle")
    .stateless
    .render_P(p =>
      Divider(as         = "h4",
              horizontal = true,
              clazz =
                SeqexecStyles.titleRow |+| SeqexecStyles.notInMobile |+| SeqexecStyles.header)(
        s"Seqexec ${p.site.shortName}",
        p.ws.ws.renderPending(_ =>
          <.div(
            SeqexecStyles.errorText,
            SeqexecStyles.blinking,
            "Connection lost"
          )
        )
      )
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

}

final case class SeqexecMain(site: Site, ctl: RouterCtl[SeqexecPages]) extends ReactProps {
  @inline def render: VdomElement = SeqexecMain.component(this)
}

object SeqexecMain {
  type Props = SeqexecMain

  implicit val propsReuse: Reusability[Props] = Reusability.by(_.site)

  private val lbConnect               = SeqexecCircuit.connect(_.uiModel.loginBox)
  private val logConnect              = SeqexecCircuit.connect(_.uiModel.globalLog)
  private val userNotificationConnect = SeqexecCircuit.connect(_.uiModel.notification)
  private val headerSideBarConnect    = SeqexecCircuit.connect(SeqexecCircuit.headerSideBarReader)
  private val wsConnect               = SeqexecCircuit.connect(_.ws)

  private val component = ScalaComponent
    .builder[Props]("SeqexecUI")
    .stateless
    .render_P(p =>
      React.Fragment(
        Grid(padded     = GridPadded.Horizontally)(
          GridRow(clazz = SeqexecStyles.shorterRow),
          wsConnect(ws => AppTitle(p.site, ws())),
          GridRow(clazz = SeqexecStyles.shorterRow |+| SeqexecStyles.queueAreaRow)(
            GridColumn(mobile   = Sixteen,
                       tablet   = Ten,
                       computer = Ten,
                       clazz    = SeqexecStyles.queueArea)(
              SessionQueueTableSection(p.ctl)
            ),
            GridColumn(tablet = Six, computer = Six, clazz = SeqexecStyles.headerSideBarArea)(
              headerSideBarConnect(x => HeadersSideBar(x()))
            )
          ),
          GridRow(clazz = SeqexecStyles.shorterRow)(
            TabsArea(p.ctl, p.site)
          ),
          GridRow(clazz = SeqexecStyles.logArea)(
            logConnect(l => LogArea(p.site, l()))
          )
        ),
        lbConnect(p => LoginBox(p())),
        userNotificationConnect(p => UserNotificationBox(UserNotificationBox.Props(p()))),
        Footer(p.ctl, p.site)
      )
    )
    .configure(Reusability.shouldComponentUpdate)
    .build

}
