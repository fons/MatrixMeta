package com.kabouterlabs.matrix

/**
  * Created by fons on 4/27/16.
  */
trait SerializeT[U] {
  def csvWrite(fn:String, u:U):Unit
  def csvRead(fn:String):U
}
