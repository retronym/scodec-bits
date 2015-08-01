package scodec.bits

import language.implicitConversions

case class Bits(value: Long) extends AnyVal with Ordered[Bits] {
  def toBytes: Bytes = Bytes((value + 7) / 8)
  def +(that: Bits): Bits = Bits(value + that.value)
  def -(that: Bits): Bits = Bits(value - that.value)
  def *(that: Bits): Bits = Bits(value * that.value)
  def /(that: Bits): Bits = Bits(value / that.value)
  def %(that: Bits): Bits = Bits(value % that.value)
  def min(that: Bits): Bits = Bits(value min that.value)
  def max(that: Bits): Bits = Bits(value max that.value)
  def toInt: Int = value.toInt
  def compare(that: Bits): Int = value.compare(that.value)
  override def toString = value + " bits"
}

object Bits {
  implicit def fromLong(l: Long): Bits = Bits(l)
  implicit def fromBytes(bc: Bytes): Bits = bc.toBits
}
