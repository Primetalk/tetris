package ru.primetalk.tetris

import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode

import scala.util.Random
import scala.scalajs.js.timers._

// Mutable part of the program
class Controller(val ctx: dom.CanvasRenderingContext2D) {
  val view = new View(ctx)
  // The following are the only two variables in the program
  val rnd = new Random()
  private var currentGameState: Game.State = Game.startGame(rnd.nextInt())

  ctx.canvas.onkeydown = { e =>
    convertKeyboardEventToGameEvent(e)
      .foreach { evt =>
        e.preventDefault()
        handleEvent(evt)
      }
  }

  def handleEvent(evt: Game.Event): Unit = {
    val oldState = currentGameState
    val newState = Game.handleEvent(currentGameState, evt, rnd.nextInt())
    view.redrawGame(oldState, newState)
    currentGameState = newState
  }

  def convertKeyboardEventToGameEvent(e: dom.KeyboardEvent): Option[Game.Event] = {
    e.keyCode match {
      case KeyCode.Space => Some(Game.UserInteraction(Game.Control.RotateBy(Game.rotateRight)))
      case KeyCode.Up => Some(Game.UserInteraction(Game.Control.RotateBy(Game.rotateLeft)))
      case KeyCode.Left => Some(Game.UserInteraction(Game.Control.ShiftXBy(-1)))
      case KeyCode.Right => Some(Game.UserInteraction(Game.Control.ShiftXBy(1)))
      case KeyCode.Down => Some(Game.UserInteraction(Game.Control.Drop))
      case KeyCode.P => Some(Game.Pause)
      case _ => None
    }
  }

  def planNextTimer(): Unit = {
    currentGameState match {
      case Game.RunningGameState(_, Game.MovingState(_, _, _, tPerYRow), _, _) =>
        setTimeout(tPerYRow) {
          handleEvent(Game.Timer)
          planNextTimer()
        }
      case Game.FinishedGame(_) => // nothing
      case Game.PausedGame(_) =>
        setTimeout(Game.defaultTPerYRow) { // we'll tick even in the paused state
          handleEvent(Game.Timer)
          planNextTimer()
        }
    }
  }

  planNextTimer()
}
