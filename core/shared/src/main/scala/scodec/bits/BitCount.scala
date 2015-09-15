package scodec.bits

import language.implicitConversions

case class BitCount(value: Long) extends AnyVal with Ordered[BitCount] {
  def toBytes: ByteCount = ByteCount((value + 7) / 8)
  def +(that: BitCount): BitCount = BitCount(value + that.value)
  def -(that: BitCount): BitCount = BitCount(value - that.value)
  def *(that: BitCount): BitCount = BitCount(value * that.value)
  def /(that: BitCount): BitCount = BitCount(value / that.value)
  def %(that: BitCount): BitCount = BitCount(value % that.value)
  def min(that: BitCount): BitCount = BitCount(value min that.value)
  def max(that: BitCount): BitCount = BitCount(value max that.value)
  def abs: BitCount = BitCount(value.abs)
  def compare(that: BitCount): Int = value.compare(that.value)
  def toLong: Long = value
  def toInt: Int = if (value <= Int.MaxValue) value.toInt else throw new IllegalStateException(s"$value too big to convert to an integer")
  override def toString = value + " bits"
}

object BitCount {
  implicit def fromLong(l: Long): BitCount = BitCount(l)
  implicit def fromBytes(bc: ByteCount): BitCount = bc.toBits
}
