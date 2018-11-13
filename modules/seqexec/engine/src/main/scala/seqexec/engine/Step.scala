// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import seqexec.model.StepState

import cats.implicits._
import monocle.Lens
import monocle.macros.GenLens

/**
  * A list of `Executions` grouped by observation.
  */
final case class Step[F[_]](
  id: Step.Id,
  breakpoint: Step.BreakpointMark,
  skipped: Step.Skipped,
  skipMark: Step.SkipMark,
  executions: List[List[Action[F]]]
)

object Step {

  type Id = Int

  final case class BreakpointMark(self: Boolean) extends AnyVal
  final case class SkipMark(self: Boolean) extends AnyVal
  final case class Skipped(self: Boolean) extends AnyVal

  def init[F[_]](id: Id,
           executions: List[List[Action[F]]]): Step[F] = Step(id, BreakpointMark(false),
    Skipped(false), SkipMark(false), executions)

  /**
    * Calculate the `Step` `Status` based on the underlying `Action`s.
    */
  def status[F[_]](step: Step[F]): StepState = {

    if(step.skipped.self) StepState.Skipped
    else
      // Find an error in the Step
      step.executions.flatten.find(Action.errored).flatMap { x => x.state.runState match {
        case Action.Failed(Result.Error(msg)) => msg.some
        case _                                => None
        // Return error or continue with the rest of the checks
      }}.map(StepState.Failed).getOrElse(
        // All actions in this Step were completed successfully, or the Step is empty.
        if (step.executions.flatten.forall(Action.completed)) StepState.Completed
        else if (step.executions.flatten.forall(_.state.runState.isIdle)) StepState.Pending
        // Not all actions are completed or pending.
        else StepState.Running
      )

  }

  /**
    * Step Zipper. This structure is optimized for the actual `Step` execution.
    *
    */
  final case class Zipper[F[_]](
    id: Int,
    breakpoint: BreakpointMark,
    skipMark: SkipMark,
    pending: List[Actions[F]],
    focus: Execution[F],
    done: List[Actions[F]],
    rolledback: (Execution[F], List[Actions[F]])
  ) { self =>

    /**
      * Adds the `Current` `Execution` to the list of completed `Execution`s and
      * makes the next pending `Execution` the `Current` one.
      *
      * If there are still `Action`s that have not finished in `Current` or if
      * there are no more pending `Execution`s it returns `None`.
      */
    val next: Option[Zipper[F]] =
      pending match {
        case Nil           => None
        case exep :: exeps =>
          (Execution.currentify(exep), focus.uncurrentify).mapN (
            (curr, exed) => self.copy(pending = exeps, focus = curr, done = exed :: done)
          )
      }

    def rollback: Zipper[F] =
      self.copy(pending = rolledback._2, focus = rolledback._1, done = Nil)

    /**
      * Obtain the resulting `Step` only if all `Execution`s have been completed.
      * This is a special way of *unzipping* a `Zipper`.
      *
      */
    val uncurrentify: Option[Step[F]] =
      if (pending.isEmpty) focus.uncurrentify.map(
        x => Step(id, breakpoint, Skipped(false), skipMark, x :: done)
      )
      else None

    /**
      * Unzip a `Zipper`. This creates a single `Step` with either completed
      * `Exection`s or pending `Execution`s.
      */
    val toStep: Step[F] =
      Step(
        id,
        breakpoint,
        Skipped(false),
        skipMark,
        done ++ List(focus.execution) ++ pending
      )

    val skip: Step[F] = toStep.copy(skipped = Skipped(true))

    def update(executions: List[Actions[F]]): Zipper[F] =
      Zipper.calcRolledback(executions).map{ case r@(_, exes) =>
        // Changing `pending` allows to propagate changes to non executed `executions`, even if the step is running
        // Don't do it if the number of executions changes. In that case the update will only have an effect if
        // the step is (re)started.
        if (exes.length === done.length + pending.length) this.copy(pending = exes.takeRight(pending.length), rolledback = r)
        else this.copy(rolledback = r)
      }.getOrElse(this)

  }

  object Zipper {

    private def calcRolledback[F[_]](executions: List[Actions[F]]): Option[(Execution[F], List[Actions[F]])
      ] = executions match {
      case Nil => None
      case exe :: exes =>
        Execution.currentify(exe).map((_, exes))
    }

    /**
      * Make a `Zipper` from a `Step` only if all the `Execution`s in the `Step` are
      * pending. This is a special way of *zipping* a `Step`.
      *
      */
    def currentify[F[_]](step: Step[F]): Option[Zipper[F]] =
      calcRolledback(step.executions).map{ case (x, exes) =>
        Zipper(
          step.id,
          step.breakpoint,
          step.skipMark,
          exes,
          x,
          Nil,
          (x, exes)
        )
      }

    def current[F[_]]: Lens[Zipper[F], Execution[F]] =
      GenLens[Zipper[F]](_.focus)

  }

}
