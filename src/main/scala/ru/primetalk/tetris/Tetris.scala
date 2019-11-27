package ru.primetalk.tetris

import org.scalajs.dom
import org.scalajs.dom.ext.Color
import org.scalajs.dom.raw.HTMLCanvasElement

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

//@JSExport
object Tetris extends Rules {
  @JSExportTopLevel("startTetris")
  def startTetris(canvasId: String): Unit = {
    val canvas = dom.document.getElementById(canvasId).asInstanceOf[HTMLCanvasElement]
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val controller = new Controller(ctx)
//    ctx.fillStyle = Color.Magenta.toString()
//    ctx.fillRect(0, 0, 100,100)
  }
  def main(args: Array[String]): Unit = {
    println("Hello, World")
  }
}
