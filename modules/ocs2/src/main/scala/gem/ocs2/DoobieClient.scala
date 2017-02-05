package gem.ocs2

import doobie.contrib.postgresql.syntax._
import doobie.imports._

import java.util.logging.{Level, Logger}

import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import scalaz.effect.IO

trait DoobieClient {
  val xa = DriverManagerTransactor[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql:gem",
    "postgres",
    ""
  )

  val lxa = DriverManagerTransactor[Task](
    "org.postgresql.Driver",
    "jdbc:postgresql:gem",
    "postgres",
    ""
  )

  def configureLogging(): Unit = List(
    "edu.gemini.spModel.type.SpTypeUtil"
  ).map(Logger.getLogger).foreach(_.setLevel(Level.OFF))

  def ignoreUniqueViolation(fa: ConnectionIO[Int]): ConnectionIO[Int] =
    for {
      s <- HC.setSavepoint
      n <- fa.onUniqueViolation(HC.rollback(s).as(0))
      _ <- HC.releaseSavepoint(s)
    } yield n

}
