package com.kabouterlabs.matrix

/**
  * Created by fons on 3/31/16.
  */
trait EigenAccessT[U] {
  def values():U
  def vectors():U
}
