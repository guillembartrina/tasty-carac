
package simple

//class SimpleString(var theString: String):
//  def +=(string: String): Unit =
//    theString = theString + string
//  def =-(sep: String): String =
//    val idx = theString.lastIndexOf(sep)
//    val res = theString.substring(idx + sep.length())
//    theString = theString.substring(0, idx)
//    res

// ---

class Ignore extends scala.annotation.StaticAnnotation

class _String(var inner: String):
  def +=(sep: String, string: _String): Unit =
    inner = inner + sep + string.inner
  def =-(sep: String): _String =
    val idx = inner.lastIndexOf(sep)
    val res = _String.empty
    res.inner = inner.substring(idx + sep.length())
    inner = inner.substring(0, idx)
    res

object _String:
  @Ignore def empty: _String = new _String("")


object Primitive:
  @Ignore def serializeBoolean(b: Boolean): _String = _String(String.valueOf(b))
  @Ignore def deserializeBoolean(sb: _String): Boolean = sb.inner.toBoolean

  @Ignore def serializeInt(i: Int): _String = _String(String.valueOf(i))
  @Ignore def deserializeInt(sb: _String): Int = sb.inner.toInt

  @Ignore def serializeLong(l: Long): _String = _String(String.valueOf(l))
  @Ignore def deserializeLong(sb: _String): Long = sb.inner.toLong

  @Ignore def serializeFloat(f: Float): _String = _String(String.valueOf(f))
  @Ignore def deserializeFloat(sb: _String): Float = sb.inner.toFloat
  
  @Ignore def serializeDouble(d: Double): _String = _String(String.valueOf(d))
  @Ignore def deserializeDouble(sb: _String): Double = sb.inner.toDouble

  @Ignore def serializeChar(c: Char): _String = _String(String.valueOf(c))
  @Ignore def deserializeChar(sb: _String): Char = sb.inner.head

  @Ignore def serializeString(s: String): _String = _String(s)
  @Ignore def deserializeString(sb: _String): String = sb.inner

import Primitive.*


def trivialSerializer(i: Int): _String = serializeInt(i)
def trivialDeserializer(sb: _String): Int = deserializeInt(sb)


case class CaseClass(i: Int, d: Double, s: String)

def serializer(cc: CaseClass): _String =
  val sb = _String.empty
  sb.+=("", serializeInt(cc.i))
  sb.+=(";", serializeDouble(cc.d))
  sb.+=(";", serializeString(cc.s))
  sb

def deserializer(sb: _String): CaseClass =
  val is = sb.=-(";")
  val i = deserializeInt(is)
  val ds = sb.=-(";")
  val d = deserializeDouble(ds)
  val ss = sb.=-("")
  val s = deserializeString(ss)
  new CaseClass(i, d, s)


def main(): Unit =
  val x = "XXX"
  val y = x
  
