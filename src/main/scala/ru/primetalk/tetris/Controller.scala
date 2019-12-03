package ru.primetalk.tetris

import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode

import scala.util.Random
import scala.scalajs.js.timers

// Mutable part of the program
class Controller(val ctx: dom.CanvasRenderingContext2D) {
  val view = new View(ctx)
  // The following are the only two variables in the program
  private val rnd = new Random()
  private var currentGameState: Game.GameState = Game.startGame(rnd.nextInt())

  def subscribeKeyboard(): Unit = {
    ctx.canvas.onkeydown = { e =>
      convertKeyboardEventToGameEvent(e)
        .foreach { evt =>
          e.preventDefault()
          handleEvent(evt)
        }
    }
  }

  def handleEvent(evt: Game.Event): Unit = {
    val oldState = currentGameState
    val newState = Game.handleEvent(oldState, evt, rnd.nextInt())
    currentGameState = newState
    view.redrawGame(oldState, newState)
  }

  def convertKeyboardEventToGameEvent(e: dom.KeyboardEvent): Option[Game.Event] = {
    e.keyCode match {
      case KeyCode.Space => Some(Game.Event.MovingShapeControl(Game.Control.RotateBy(Game.rotateRight)))
      case KeyCode.Up    => Some(Game.Event.MovingShapeControl(Game.Control.RotateBy(Game.rotateLeft)))
      case KeyCode.Left  => Some(Game.Event.MovingShapeControl(Game.Control.ShiftXBy(-1)))
      case KeyCode.Right => Some(Game.Event.MovingShapeControl(Game.Control.ShiftXBy(1)))
      case KeyCode.Down  => Some(Game.Event.MovingShapeControl(Game.Control.Drop))
      case KeyCode.P     => Some(Game.Event.Pause)
      case _ => None
    }
  }

  def planNextTimer(): Unit = {
    currentGameState match {
      case Game.GameState.Running(_, Game.MovingState(_, _, _, tPerYRow), _, _) =>
        timers.setTimeout(tPerYRow) {
          handleEvent(Game.Event.Timer)
          planNextTimer()
        }
      case Game.GameState.Finished(_) => // nothing
      case Game.GameState.Paused(_) =>
        timers.setTimeout(Game.defaultTPerYRow) { // we'll tick even in the paused state
          handleEvent(Game.Event.Timer)
          planNextTimer()
        }
    }
  }

  // automatically subscribe on all events upon construction
  subscribeKeyboard()
  planNextTimer()
}
