package com.kabouterlabs.matrix

import java.io.{PrintWriter, StringWriter}

/**
  * Created by fons on 3/30/16.
  */
case class EigenResultM[U](result:Option[U]) {

}


object EigenResultM {
  def none[U] = new EigenResultM[U](None)

  def alloc[U](u:Option[U]):EigenResultM[U] = {
    try {
      new EigenResultM[U](u)
    }
    catch {
      case e: Throwable => {
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        println("exception caught :" + e + sw)
        new EigenResultM[U](None)
      }

    }
  }
  def alloc[U](f: () => U): EigenResultM[U] = {
    try {
      new EigenResultM[U](Some(f()))
    }
    catch {
      case e: Throwable => {
        val sw = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        println("exception caught :" + e + sw)
        new EigenResultM[U](None)
      }

    }
  }
}