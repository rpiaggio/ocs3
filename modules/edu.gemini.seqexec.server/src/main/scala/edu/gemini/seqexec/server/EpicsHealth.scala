// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

/**
  * Created by jluhrs on 7/18/17.
  */
sealed trait EpicsHealth

object EpicsHealth {
  object Good extends EpicsHealth
  object Bad extends EpicsHealth
  implicit def fromInt(v: Int): EpicsHealth = if(v == 0) Good else Bad
}
