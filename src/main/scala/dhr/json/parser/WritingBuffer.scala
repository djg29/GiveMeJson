package dhr.json.parser

import java.io.Writer

class WritingBuffer(writer: Writer, bufferSize: Int) extends Writer {

  val buffer: Array[Char] = new Array[Char](bufferSize)
  var fill: Int = _

  def this(writer: Writer) = {
    this(writer, 16)
  }

  /**
    * Flushes the internal buffer but does not flush the wrapped writer.
    */
  override def flush(): Unit = {
    writer.write(buffer, 0, fill)
    fill = 0
  }

  /**
    * Does not close or flush the wrapped writer.
    */
  override def close(): Unit = {}

  override def write(cbuf: Array[Char], off: Int, len: Int): Unit = {
    if (fill > buffer.length - len) {
      flush()
      if (len > buffer.length) {
        writer.write(cbuf, off, len)
        return
      }
    }
    System.arraycopy(cbuf, off, buffer, fill, len)
    fill += len
  }

  override def write(c: Int): Unit = {
    if (fill > buffer.length - 1) {
      flush()
    }
    fill += 1
    buffer(fill) = c.toChar
  }

  override def write(str: String, off: Int, len: Int): Unit = {
    if (fill > buffer.length - len) {
      flush()
      if (len > buffer.length) {
        writer.write(str, off, len)
        return
      }
    }
    str.getChars(off, off + len, buffer, fill)
    fill += len
  }

}
