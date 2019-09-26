package web.client.facades.aggrid

import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Js.{RawMounted, UnmountedMapped}
import japgolly.scalajs.react.internal.Effect.Id
import japgolly.scalajs.react.vdom.VdomElement
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSImport, JSName}
import scala.scalajs.js.|

object AgGridReact {

  @js.native
  @JSImport("ag-grid-react", "AgGridReact")
  object RawComponent extends js.Object

  @js.native
  @JSImport("ag-grid-enterprise", JSImport.Namespace)
  object EnableEnterprise extends js.Object

  @js.native
  trait JsMethods extends js.Object {
    def forceUpdateGrid(): Unit = js.native
    def getOffsetForRow(o: js.Object): Int = js.native
    def measureAllRows(): Unit = js.native
    def recomputeRowHeights(index:  Int): Unit = js.native
    def scrollToPosition(scrollTop: Int): Unit = js.native
    def scrollToRow(index:          Int): Unit = js.native
  }

  // <ColDef>
  // https://www.ag-grid.com/javascript-grid-column-properties/
  trait ColDef extends js.Object {
    val headerName: js.UndefOr[String] = js.undefined
  }

  trait SingleColDef[T] extends ColDef {
    val field: js.UndefOr[String] = js.undefined
    val cellRendererFramework: js.UndefOr[raw.React.ComponentType[CellRendererParams[T]]] =
      js.undefined
    val width: js.UndefOr[Int]         = js.undefined
    val minWidth: js.UndefOr[Int]      = js.undefined
    val maxWidth: js.UndefOr[Int]      = js.undefined
    val resizable: js.UndefOr[Boolean] = js.undefined
    val rowDrag: js.UndefOr[Boolean]   = js.undefined
  }

  trait ColGroupDef extends ColDef {
    val children: js.UndefOr[js.Array[ColDef]] = js.undefined
  }
  // </ColDef>

  sealed trait Enum {
    val name: String = {
      val (h, t) = toString.splitAt(1)
      h.toLowerCase + t
    }
  }

  sealed trait RowModel extends Enum
  object RowModel {
    case object ClientSide extends RowModel
    case object Infinite extends RowModel
    case object ServerSide extends RowModel // Enterprise
    case object Viewport extends RowModel // Enterprise
  }

  sealed trait RowSelection extends Enum
  object RowSelection {
    case object Single extends RowSelection
    case object Multiple extends RowSelection
  }

  @js.native
  trait GetRowsParams[T] extends js.Object {
    var startRow: Int
    var endRow: Int
    var context: js.Object
    def successCallback(rowsThisBlock: js.Array[T], lastRow: js.UndefOr[Int] = js.undefined): Unit
    def failCallback(): Unit
  }

  // https://www.ag-grid.com/javascript-grid-refresh/#refresh-cells
  class RefreshCellsParams[T](val rowNodes: js.UndefOr[js.Array[RowNode[T]]]      = js.undefined,
                              val columns:  js.UndefOr[js.Array[String | Column]] = js.undefined,
                              val force:    js.UndefOr[Boolean]                   = js.undefined)
    extends js.Object

  abstract class DataSource[T] extends js.Object {
    def getRows(params: GetRowsParams[T]): Unit
    def destroy(): Unit = {}
  }

  // <RowNode>
  // https://www.ag-grid.com/javascript-grid-row-node/
  @js.native
  trait RowNode[T] extends js.Object {
    val id: String
    val data: T
    val parent: js.UndefOr[RowNode[_]]
    val level: Int
    val uiLevel: Int
    val group: Boolean
    val firstChild: Boolean
    val lastChild: Boolean
    val childIndex: Int
    val rowPinned: js.UndefOr[String]
    val canFlower: Boolean
    val childFlower: js.Any // Type can probably be narrowed down
    val stub: Boolean // Enterprise
    val rowHeight: Int
    val rowTop: Int
    val quickFilterAggregateText: js.UndefOr[String]

    def setRowHeight(height: Int): Unit
    def setData(newData:     T): Unit
  }
  // </RowNode>

  // <GridAPI>
  // https://www.ag-grid.com/javascript-grid-api/
  @js.native
  trait GridApi extends js.Object {
    // Columns
    def sizeColumnsToFit(): Unit = js.native

    // Selection
    def refreshCells[T](params: RefreshCellsParams[T]): Unit = js.native

    // Scrolling
    def ensureIndexVisible(index: Int, position: String): Unit = js.native

    // Miscellaneous
    def resetRowHeights(): Unit    = js.native
    def onRowHeightChanged(): Unit = js.native
  }
  // </GridAPI>

  // <ColumnAPI>
  // https://www.ag-grid.com/javascript-grid-column-api/
  @js.native
  trait ColumnApi extends js.Object {}
  // </ColumnAPI>

  // <Column>
  // https://www.ag-grid.com/javascript-grid-column/
  @js.native
  trait Column extends js.Object {}
  // </Column>

  // https://www.ag-grid.com/javascript-grid-row-height/#getrowheight-callback
  @js.native
  trait GetRowHeightParams[T] extends js.Object {
    val node: RowNode[T]
    val data: T
    val api: GridApi
    val context: js.UndefOr[js.Object]
  }

  // https://www.ag-grid.com/javascript-grid-master-detail/
  @js.native
  trait DetailCellRendererParams[S] extends js.Object {
    var detailGridOptions: Props
    var getDetailRowData: js.Function1[GetRowsParams[S], Unit]
  }
  object DetailCellRendererParams {
    def apply[S](
                  detailGridOptions: Props,
                  getDetailRowData:  GetRowsParams[S] => Callback
                ): DetailCellRendererParams[_] = {
      val p = (new js.Object).asInstanceOf[DetailCellRendererParams[_]]
      p.detailGridOptions = detailGridOptions
      p.getDetailRowData = { params: GetRowsParams[_] =>
        getDetailRowData(params.asInstanceOf[GetRowsParams[S]]).runNow()
      }
      p
    }
  }

  // <Events>
  @js.native
  trait AgEvent extends js.Object {
    @JSName("type")
    val tpe: String
  }

  @js.native
  trait AgGridEvent extends AgEvent {
    val api: GridApi
    val columnApi: ColumnApi
  }

  @js.native
  trait GridReadyEvent extends AgGridEvent

  @js.native
  trait RowEvent[T] extends AgGridEvent {
    val node: RowNode[T]
    val data: T
    val rowIndex: Int
    val rowPinned: js.UndefOr[String]
    val context: js.UndefOr[js.Object]
    val event: js.UndefOr[dom.Event]
  }

  @js.native
  trait CellEvent[T] extends RowEvent[T] {
    val column: Column
    val colDef: SingleColDef[T]
    val value: js.Any
  }

  @js.native
  trait CellClickedEvent[T] extends CellEvent[T]

  @js.native
  trait RowDragEvent[T] extends AgGridEvent {
    val event: dom.MouseEvent
    val node: RowNode[T]
    val overIndex: Int
    val overNode: RowNode[T]
    val y: Int
    val vDirection: js.UndefOr[String]
  }

  @js.native
  trait RowDragEnterEvent[T] extends RowDragEvent[T]

  @js.native
  trait RowDragMoveEvent[T] extends RowDragEvent[T]

  @js.native
  trait RowDragEndEvent[T] extends RowDragEvent[T]

  @js.native
  trait RowDragLeaveEvent[T] extends RowDragEvent[T]
  // </Events>

  @js.native
  trait CellRendererParams[T] extends js.Object {
    val value: js.Any // value to be rendered
    val valueFormatted: js.Any // value to be rendered formatted
    val getValue: Unit => js.Any // convenience function to get most recent up to date value
    val setValue: js.Any => Unit // convenience to set the value
    val formatValue: js.Any => js.Any // convenience to format a value using the columns formatter
    val data: js.UndefOr[T] // the rows data
    val node: RowNode[T] // row rows row node
    val colDef: SingleColDef[T] // the cells column definition
    val column: Column // the cells column
    val rowIndex: Int // the current index of the row (changes after filter and sort)
    val api: GridApi // the grid API
    val eGridCell: dom.raw.HTMLElement // the grid's cell, a DOM div element
    val eParentOfValue: dom.raw.HTMLElement // the parent DOM item for the cell renderer, same as eGridCell unless using checkbox selection
    val columnApi: ColumnApi // grid column API
    val context: js.Any // the grid's context
    val refreshCell: Unit => Unit // convenience function to refresh the cell
  }

  // <Props>
  // https://www.ag-grid.com/javascript-grid-properties/
  @js.native
  trait Props extends js.Object {
    // Columns
    var columnDefs: js.UndefOr[js.Array[ColDef]]
    var defaultColDef: js.UndefOr[SingleColDef[_]]
    var defaultColGroupDef: js.UndefOr[ColGroupDef]

    // Selection
    var rowSelection: js.UndefOr[String]

    // Row Dragging
    var rowDragManaged: js.UndefOr[Boolean]

    // Data & Row Models
    var rowModelType: js.UndefOr[String]
    var rowData: js.UndefOr[js.Array[_]]
    var datasource: js.UndefOr[DataSource[_]]
    var cacheBlockSize: js.UndefOr[Int]

    // Master Detail
    var masterDetail: js.UndefOr[Boolean] // Enterprise
    var detailCellRendererParams: js.UndefOr[DetailCellRendererParams[_]] // Enterprise

    // Rendering & Styling
    var rowHeight: js.UndefOr[Int]
    var animateRows: js.UndefOr[Boolean]

    // Events
    var onGridReady: js.UndefOr[js.Function1[GridReadyEvent, Unit]]
    var onCellClicked: js.UndefOr[js.Function1[CellClickedEvent[_], Unit]]
    var onRowDragEnter: js.UndefOr[js.Function1[RowDragEnterEvent[_], Unit]]
    var onRowDragMove: js.UndefOr[js.Function1[RowDragMoveEvent[_], Unit]]
    var onRowDragEnd: js.UndefOr[js.Function1[RowDragEndEvent[_], Unit]]
    var onRowDragLeave: js.UndefOr[js.Function1[RowDragLeaveEvent[_], Unit]]

    // Callbacks
    var getRowHeight: js.UndefOr[js.Function1[GetRowHeightParams[_], Int]]
  }
  // </Props>

  object Props {
    implicit class PropsOps(p: Props) {
      @inline def render: VdomElement = component.apply(p)()
    }
  }

  private def jsFun[E, E1, R]
    (h: js.UndefOr[E => CallbackTo[R]]): js.UndefOr[js.Function1[E1, R]] =
      h.map(someH => { e: E1 =>
        someH(e.asInstanceOf[E]).runNow()
      })

  private def jsEvent[E <: AgEvent, E1 <: AgEvent]
    (h: js.UndefOr[E => Callback]): js.UndefOr[js.Function1[E1, Unit]] =
      jsFun[E, E1, Unit](h)

  def props[T /* <: js.Object*/ ]( // The row type has to <: js.Object if you want to use AgGrid's renderers, but we don't enforce it.
     columnDefs:               js.UndefOr[js.Array[ColDef]]                         = js.undefined,
     defaultColDef:            js.UndefOr[SingleColDef[T]]                          = js.undefined,
     defaultColGroupDef:       js.UndefOr[ColGroupDef]                              = js.undefined,
     rowSelection:             js.UndefOr[RowSelection]                             = js.undefined,
     rowDragManaged:           js.UndefOr[Boolean]                                  = js.undefined,
     rowModelType:             js.UndefOr[RowModel]                                 = js.undefined,
     rowData:                  js.UndefOr[js.Array[T]]                              = js.undefined,
     datasource:               js.UndefOr[DataSource[T]]                            = js.undefined,
     cacheBlockSize:           js.UndefOr[Int]                                      = js.undefined,
     masterDetail:             js.UndefOr[Boolean]                                  = js.undefined,
     detailCellRendererParams: js.UndefOr[DetailCellRendererParams[_]]              = js.undefined,
     rowHeight:                js.UndefOr[Int]                                      = js.undefined,
     animateRows:              js.UndefOr[Boolean]                                  = js.undefined,
     onGridReady:              js.UndefOr[GridReadyEvent => Callback]               = js.undefined,
     onCellClicked:            js.UndefOr[CellClickedEvent[T] => Callback]          = js.undefined,
     onRowDragEnter:           js.UndefOr[RowDragEnterEvent[T] => Callback]         = js.undefined,
     onRowDragMove:            js.UndefOr[RowDragMoveEvent[T] => Callback]          = js.undefined,
     onRowDragEnd:             js.UndefOr[RowDragEndEvent[T] => Callback]           = js.undefined,
     onRowDragLeave:           js.UndefOr[RowDragLeaveEvent[T] => Callback]         = js.undefined,
     getRowHeight:             js.UndefOr[GetRowHeightParams[T] => CallbackTo[Int]] = js.undefined,
   ): Props = {
    val p = (new js.Object).asInstanceOf[Props]
    p.columnDefs               = columnDefs
    p.defaultColDef            = defaultColDef
    p.defaultColGroupDef       = defaultColGroupDef
    p.rowSelection             = rowSelection.map(_.name)
    p.rowDragManaged           = rowDragManaged
    p.rowModelType             = rowModelType.map(_.name)
    p.rowData                  = rowData
    p.datasource               = datasource
    p.cacheBlockSize           = cacheBlockSize
    p.masterDetail             = masterDetail
    p.detailCellRendererParams = detailCellRendererParams
    p.rowHeight                = rowHeight
    p.animateRows              = animateRows
    p.onGridReady              = jsEvent[GridReadyEvent, GridReadyEvent](onGridReady)
    p.onCellClicked            = jsEvent[CellClickedEvent[T], CellClickedEvent[_]](onCellClicked)
    p.onRowDragEnter           = jsEvent[RowDragEnterEvent[T], RowDragEnterEvent[_]](onRowDragEnter)
    p.onRowDragMove            = jsEvent[RowDragMoveEvent[T], RowDragMoveEvent[_]](onRowDragMove)
    p.onRowDragEnd             = jsEvent[RowDragEndEvent[T], RowDragEndEvent[_]](onRowDragEnd)
    p.onRowDragLeave           = jsEvent[RowDragLeaveEvent[T], RowDragLeaveEvent[_]](onRowDragLeave)
    p.getRowHeight             = jsFun[GetRowHeightParams[T], GetRowHeightParams[_], Int](getRowHeight)
    p
  }

  val component = JsComponent[Props, Children.Varargs, Null](RawComponent)
    .addFacade[JsMethods]

  def apply(p: Props)
  : UnmountedMapped[Id, Props, Null, RawMounted[Props, Null] with JsMethods, Props, Null] =
    component.apply(p)()
}
