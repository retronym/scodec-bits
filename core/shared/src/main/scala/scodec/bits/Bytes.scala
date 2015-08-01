package scodec.bits

import language.implicitConversions

case class Bytes(value: Long) extends AnyVal with Ordered[Bytes] {
  def toBits: Bits = Bits(value * 8)
  def +(that: Bytes): Bytes = Bytes(value + that.value)
  def -(that: Bytes): Bytes = Bytes(value - that.value)
  def min(that: Bytes): Bytes = Bytes(value min that.value)
  def max(that: Bytes): Bytes = Bytes(value max that.value)
  def compare(that: Bytes): Int = value.compare(that.value)
  override def toString = value + " bytes"
}

object Bytes {
  implicit def fromLong(l: Long): Bytes = Bytes(l)
  implicit def fromBits(bc: Bits): Bytes = bc.toBytes
}
