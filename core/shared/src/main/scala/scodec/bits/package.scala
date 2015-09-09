package scodec

import scala.language.experimental.macros
import scala.language.implicitConversions

/**
 * Provides immutable data types for working with bits and bytes.
 *
 * @see [[BitVector]] and [[ByteVector]]
 */
package object bits {

  /**
   * Provides the `bin` string interpolator, which returns `BitVector` instances from binary strings.
   */
  final implicit class BinStringSyntax(val sc: StringContext) extends AnyVal {

    /**
     * Converts this binary literal string to a `BitVector`. Whitespace characters are ignored.
     *
     * Named arguments are supported in the same manner as the standard `s` interpolator but they must be
     * of type `BitVector`.
     */
    def bin(args: BitVector*): BitVector = macro LiteralSyntaxMacros.binStringInterpolator
  }

  /**
   * Provides the `hex` string interpolator, which returns `ByteVector` instances from hexadecimal strings.
   */
  final implicit class HexStringSyntax(val sc: StringContext) extends AnyVal {

    /**
     * Converts this hexadecimal literal string to a `ByteVector`. Whitespace characters are ignored.
     *
     * Named arguments are supported in the same manner as the standard `s` interpolator but they must be
     * of type `ByteVector`.
     */
    def hex(args: ByteVector*): ByteVector = macro LiteralSyntaxMacros.hexStringInterpolator
  }

  final implicit class LongSyntax(val value: Long) extends AnyVal {
    def bit: BitCount = BitCount(value)
    def bits: BitCount = BitCount(value)
    def byte: ByteCount = ByteCount(value)
    def bytes: ByteCount = ByteCount(value)
  }
  final implicit class IntSyntax(val value: Int) extends AnyVal {
    def bit: BitCount = BitCount(value.toLong)
    def bits: BitCount = BitCount(value.toLong)
    def byte: ByteCount = ByteCount(value.toLong)
    def bytes: ByteCount = ByteCount(value.toLong)
  }
}
