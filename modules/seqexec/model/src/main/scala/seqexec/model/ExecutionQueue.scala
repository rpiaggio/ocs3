// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.implicits._
import gem.Observation
import monocle.macros.Lenses
import seqexec.model.enum.BatchCommandState

@Lenses
final case class ExecutionQueue(name:     String,
                                cmdState: BatchCommandState,
                                queue:    List[Observation.Id])

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object ExecutionQueue {
  def init(name: String): ExecutionQueue =
    ExecutionQueue(name, BatchCommandState.Idle, List())

  implicit val eq: Eq[ExecutionQueue] =
    Eq.by(x => (x.name, x.cmdState, x.queue))
}