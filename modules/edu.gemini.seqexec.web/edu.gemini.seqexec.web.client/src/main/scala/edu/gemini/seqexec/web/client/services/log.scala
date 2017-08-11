// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.services

import java.util.logging.{Handler, Level, LogRecord, SimpleFormatter}

object log {
  // Override Console Handler to use the default js console
  class ConsoleHandler(level: Level) extends Handler {
    setFormatter(new SimpleFormatter)
    setLevel(level)

    override def publish(record: LogRecord): Unit = {
      if (record.getLevel == Level.SEVERE) {
        System.err.println(getFormatter.format(record)) // scalastyle:ignore
      } else {
        println(getFormatter.format(record)) // scalastyle:ignore
      }
    }

    override def flush(): Unit = {}

    override def close(): Unit = {}
  }

  // Use AjaxHandler to post log messages to the backend
  class AjaxHandler(level: Level) extends Handler {
    setLevel(level)

    override def publish(record: LogRecord): Unit = {
      SeqexecWebClient.log(record)
      ()
    }

    override def flush(): Unit = {}

    override def close(): Unit = {}
  }
}
