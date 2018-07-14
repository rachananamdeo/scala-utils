package com.shanker.DateUtils

class UnmodifiableSeq[A](buffer: Seq[A]) extends Seq[A] {
  
  @inline @Override def update(idx: Int, elem: A) { throw new UnsupportedOperationException() }

  @inline @Override def length = buffer.length

  @inline @Override def apply(idx: Int) = buffer(idx)

  @inline @Override def iterator = buffer.iterator
}