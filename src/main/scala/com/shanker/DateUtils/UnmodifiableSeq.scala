package com.shanker.DateUtils

class UnmodifiableSeq[A](buffer: Seq[A]) extends Seq[A] {

  @inline override def length = buffer.length

  @inline override def apply(idx: Int) = buffer(idx)

  @inline override def iterator = buffer.iterator

}