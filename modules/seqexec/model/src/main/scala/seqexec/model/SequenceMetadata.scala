// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.implicits._
import monocle.macros.Lenses
import seqexec.model.enum.Instrument

/** Metadata about the sequence required on the exit point */
@Lenses
final case class SequenceMetadata(
  instrument: Instrument,
  observer:   Option[Observer],
  name:       String
)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object SequenceMetadata {
  implicit val eq: Eq[SequenceMetadata] =
    Eq.by(x => (x.instrument, x.observer, x.name))
}