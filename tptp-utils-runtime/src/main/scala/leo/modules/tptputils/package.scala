package leo.modules

import leo.datastructures.TPTP.AnnotatedFormula.FormulaType

package object tptputils {



  class TPTPTransformException(message: String) extends RuntimeException(message)

  sealed abstract class ExternalLanguage
  case object LegalRuleML extends ExternalLanguage

  /** Returns true iff `role` does not contain a subrole (i.e., role followed by dash followed by general term). */
  final def isSimpleRole(role: String): Boolean = !role.contains("-")
  /** Returns true iff `role` does contain a subrole (i.e., role followed by dash followed by general term). */
  final def isComplexRole(role: String): Boolean = !isSimpleRole(role)
  /** Returns None iff `role` is a simple role; otherwise the subrole part of the role. */
  final def getSubrole(role: String): Option[String] = if (isComplexRole(role)) {
    Some(roleSplit(role)._2.tail)
  } else None
  /** Returns the non-subrole part of the role. If no subrole-part exists, the role itself it returned. */
  final def toSimpleRole(role: String): String = roleSplit(role)._1
  // roleSplit("<prefix>-<suffix>") gives ("<prefix>", "-<suffix>"), i.e., the dash is still in there.
  private[this] final def roleSplit(role: String): (String, String) = role.span(_ != '-')

  final def isMoreGeneralThan(a: FormulaType.FormulaType, b: FormulaType.FormulaType): Boolean = {
    import FormulaType._
    a match {
      case THF => b match {
        case TPI => false
        case _ => true
      }
      case TFF => b match {
        case THF | TPI => false
        case _ => true
      }
      case FOF => b match {
        case FOF | CNF => true
        case _ => false
      }
      case TCF => b match {
        case TCF | CNF => true
        case _ => false
      }
      case CNF => b match {
        case CNF => true
        case _ => false
      }
      case TPI => false
    }
  }

  import scala.io.Source
  import java.io.InputStream
  import java.nio.charset.Charset
  import java.nio.CharBuffer
  import java.nio.ByteBuffer

  /**
   * A SourceInputStream obtains input bytes
   * from a scala.io.Source and encodes obtained character stream
   * into a stream of raw bytes using given encoding.
   *
   * @constructor create a new instance of SourceInputStream.
   * @param source underlying scala.io.Source.
   * @param encoding encoding, that will be used to transform character stream into a stream of bytes.
   * @see scala.io.Source
   * @see java.io.InputStream
   */
  class SourceInputStream(source: Source, encoding: String = "UTF-8") extends InputStream {

    private val encoder = Charset.forName(encoding).newEncoder()
    private var buffer = Array.ofDim[Byte](1024 * 16)
    private var bufferEnd = 0
    private var bufferPos = 0

    /**
     * Reads a byte of data from underlying scala.io.Source.
     *
     * @return     the next byte of data, or -1 if the end of the
     *             file is reached.
     */
    override def read: Int = {

      if (availableBytes < 1) {
        readNextChunk()
      }

      if (availableBytes < 1) return -1
      else {
        val result = buffer(bufferPos)
        bufferPos = math.min(bufferPos + 1, bufferEnd)
        result
      }

    }

    /**
     * Reads a subarray as a sequence of bytes.
     * @param b the data to be written
     * @param off the start offset in the data
     * @param len the number of bytes that are written
     */
    override def read(b: Array[Byte], off: Int, len: Int): Int = {

      require(off >= 0, "argument off of the %s.read should be >= 0".format(getClass().getName()))

      require(len > 0, "argument len of the %s.read should be > 0".format(getClass().getName()))

      val bytesNeeded = math.min(len, b.size - off)

      if (buffer.size < len * 2) resizeBuffer(len * 2)

      if (availableBytes < 1) {
        readNextChunk()
      }

      if (availableBytes < 1) -1
      else {
        if (availableBytes < bytesNeeded) {
          val firstHalfLength = availableBytes
          System.arraycopy(buffer, bufferPos, b, off, math.max(firstHalfLength, 0))
          readNextChunk()
          if (availableBytes < 0) firstHalfLength
          else {
            val secondHalfLength = math.min(availableBytes, bytesNeeded - firstHalfLength)
            System.arraycopy(buffer, bufferPos, b, off + firstHalfLength, secondHalfLength)
            bufferPos += secondHalfLength
            math.max(firstHalfLength + secondHalfLength, -1)
          }
        } else {
          System.arraycopy(buffer, bufferPos, b, off, bytesNeeded)
          bufferPos = math.min(bufferPos + bytesNeeded, bufferEnd)
          bytesNeeded
        }
      }
    }

    private def resizeBuffer(len: Int): Unit = {
      val newBuffer = Array.ofDim[Byte](len * 2)
      buffer.copyToArray(newBuffer)
      buffer = newBuffer
    }

    private def availableBytes: Int = bufferEnd - bufferPos

    private def readNextChunk(): Unit = {
      if (source.hasNext) {
        val chars = source.take(buffer.length / 2).toArray
        if (chars.length < 1) bufferEnd = bufferPos - 1
        else {
          bufferEnd = charArray2ByteArray(chars, buffer)
          bufferPos = 0
        }
      } else {
        bufferEnd = bufferPos - 1
      }
    }

    private def charArray2ByteArray(chars: Array[Char], bytes: Array[Byte]): Int = {
      val bb = ByteBuffer.wrap(bytes, 0, bytes.size)
      var cr = encoder.encode(CharBuffer.wrap(chars, 0, chars.length), bb, true)
      if (!cr.isUnderflow())
        cr.throwException();
      cr = encoder.flush(bb);
      if (!cr.isUnderflow())
        cr.throwException();
      encoder.reset()
      bb.position()
    }

  }

}
