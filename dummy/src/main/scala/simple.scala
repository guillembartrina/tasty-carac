
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

class StringManipulator:
  val sep = ";"
  var inner: String = ""
  def push(string: String): Unit =
    inner = inner + sep + string
  def pop(): String =
    val idx = inner.lastIndexOf(sep)
    val res = inner.substring(idx + sep.length())
    inner = inner.substring(0, idx)
    res
  def string(): String = inner

object StringManipulator:
  @Ignore def empty: StringManipulator = new StringManipulator()
  @Ignore def from(s: String): StringManipulator =
    val sm = new StringManipulator()
    sm.inner = s
    sm


object Primitive:
  @Ignore def serializeBoolean(b: Boolean): String = String.valueOf(b)
  @Ignore def deserializeBoolean(sb: String): Boolean = sb.toBoolean

  @Ignore def serializeInt(i: Int): String = String.valueOf(i)
  @Ignore def deserializeInt(sb: String): Int = sb.toInt

  @Ignore def serializeLong(l: Long): String = String.valueOf(l)
  @Ignore def deserializeLong(sb: String): Long = sb.toLong

  @Ignore def serializeFloat(f: Float): String = String.valueOf(f)
  @Ignore def deserializeFloat(sb: String): Float = sb.toFloat
  
  @Ignore def serializeDouble(d: Double): String = String.valueOf(d)
  @Ignore def deserializeDouble(sb: String): Double = sb.toDouble

  @Ignore def serializeChar(c: Char): String = String.valueOf(c)
  @Ignore def deserializeChar(sb: String): Char = sb.head

  @Ignore def serializeString(s: String): String = String(s)
  @Ignore def deserializeString(sb: String): String = sb

import Primitive.*


def trivialSerializer(i: Int): String = serializeInt(i)
def trivialDeserializer(sb: String): Int = deserializeInt(sb)

def trivialSerializer2(i: Int): String = trivialSerializer(i)
def trivialDeserializer2(sb: String): Int = trivialDeserializer(sb)

case class CaseClass(i: Int, d: Double, s: String)

def serializer(cc: CaseClass): String =
  val sm = StringManipulator.empty
  val d = cc.d
  sm.push(trivialSerializer2(cc.i))
  sm.push(serializeDouble(d))
  sm.push(serializeString(cc.s))
  sm.string()

def deserializer(str: String): CaseClass =
  val sm = StringManipulator.from(str)
  val s = deserializeString(sm.pop())
  val d = deserializeDouble(sm.pop())
  val i = trivialDeserializer2(sm.pop())
  new CaseClass(i, d, s)


case class CaseClassComp(cc1: CaseClass, cc2: CaseClass)

def serializerComp(ccc: CaseClassComp): String =
  val sm = StringManipulator.empty
  val d = ccc.cc1
  sm.push(serializer(d))
  sm.push(serializer(ccc.cc2))
  sm.string()

def deserializerComp(str: String): CaseClassComp =
  val sm = StringManipulator.from(str)
  val cc2 = deserializer(sm.pop())
  val cc1 = deserializer(sm.pop())
  new CaseClassComp(cc1, cc2)


def main(): Unit =
  //val i = 0
  val i = new CaseClass(0, 0.0, "str")
  val j = new CaseClassComp(i, i)
  val m = serializerComp(j)
  val o = deserializerComp(m)
  println(j)
  println(o)
  
