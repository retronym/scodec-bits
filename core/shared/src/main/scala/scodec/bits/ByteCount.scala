package scodec.bits

import language.implicitConversions

case class ByteCount(value: Long) extends AnyVal with Ordered[ByteCount] {
  def toBits: BitCount = BitCount(value * 8)
  def +(that: ByteCount): ByteCount = ByteCount(value + that.value)
  def -(that: ByteCount): ByteCount = ByteCount(value - that.value)
  def *(that: ByteCount): ByteCount = ByteCount(value * that.value)
  def /(that: ByteCount): ByteCount = ByteCount(value / that.value)
  def %(that: ByteCount): ByteCount = ByteCount(value % that.value)
  def min(that: ByteCount): ByteCount = ByteCount(value min that.value)
  def max(that: ByteCount): ByteCount = ByteCount(value max that.value)
  def abs: ByteCount = ByteCount(value.abs)
  def compare(that: ByteCount): Int = value.compare(that.value)
  def toLong: Long = value
  def toInt: Int = if (value <= Int.MaxValue) value.toInt else throw new IllegalStateException(s"$value too big to convert to an integer")
  override def toString = value + " bytes"
}

object ByteCount {
  def apply(value: Int): ByteCount = ByteCount(value.toLong)
  implicit def fromLong(l: Long): ByteCount = ByteCount(l)
  implicit def fromBits(bc: BitCount): ByteCount = bc.toBytes
}
