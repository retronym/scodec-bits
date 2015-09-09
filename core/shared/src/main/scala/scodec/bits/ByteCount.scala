package scodec.bits

import language.implicitConversions

case class ByteCount(value: Long) extends AnyVal with Ordered[ByteCount] {
  def toBits: BitCount = BitCount(value * 8)
  def +(that: ByteCount): ByteCount = ByteCount(value + that.value)
  def -(that: ByteCount): ByteCount = ByteCount(value - that.value)
  def min(that: ByteCount): ByteCount = ByteCount(value min that.value)
  def max(that: ByteCount): ByteCount = ByteCount(value max that.value)
  def compare(that: ByteCount): Int = value.compare(that.value)
  override def toString = value + " bytes"
}

object ByteCount {
  implicit def fromLong(l: Long): ByteCount = ByteCount(l)
  implicit def fromBits(bc: BitCount): ByteCount = bc.toBytes
}
