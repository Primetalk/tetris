package ru.primetalk.tetris

import org.scalajs.dom
import org.scalajs.dom.raw.HTMLCanvasElement
import ru.primetalk.presentation.{Demo1, Demo1View}

import scala.scalajs.js.annotation.JSExportTopLevel

object Tetris extends Rules {
  @JSExportTopLevel("startTetris")
  def startTetris(canvasId: String): Unit = {
    val canvas = dom.document.getElementById(canvasId).asInstanceOf[HTMLCanvasElement]
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val controller = new Controller(ctx, TetrisGame.GameImpl, new View(ctx))
  }

  @JSExportTopLevel("startPresentation")
  def startPresentation(canvasId: String): Unit = {
    val canvas = dom.document.getElementById(canvasId).asInstanceOf[HTMLCanvasElement]
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val controller = new Controller(ctx, Demo1.GameImpl, new Demo1View(ctx))
  }

  def main(args: Array[String]): Unit = {
    println("Hello, World")
  }
}
