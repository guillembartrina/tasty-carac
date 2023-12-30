
package simple

import java.nio.ByteBuffer

//class SimpleString(var theString: String):
//  def +=(string: String): Unit =
//    theString = theString + string
//  def =-(sep: String): String =
//    val idx = theString.lastIndexOf(sep)
//    val res = theString.substring(idx + sep.length())
//    theString = theString.substring(0, idx)
//    res

// ---

class Skip extends scala.annotation.StaticAnnotation
class Shallow extends scala.annotation.StaticAnnotation

class StringManipulator:
  var inner: String = ""
  def push(string: String): Unit =
    inner = inner + string + StringManipulator.encodeSize(string.size)
  def pop(): String =
    val size = StringManipulator.decodeSize(inner.takeRight(4))
    val res = inner.dropRight(4).takeRight(size)
    inner = inner.dropRight(4 + size)
    res
  def string(): String = inner

@Skip object StringManipulator:
  private def encodeSize(size: Int): String =
    List((size & 0xFF), (size >>> 8 & 0xFF), (size >>> 16 & 0xFF), (size >>> 24 & 0xFF)).map(_.toChar).mkString
  private def decodeSize(str: String): Int =
    str(0).toInt + (str(1).toInt << 8) + (str(2).toInt << 16) + (str(3).toInt << 24)

  def empty: StringManipulator = new StringManipulator()
  def from(s: String): StringManipulator =
    val sm = new StringManipulator()
    sm.inner = s
    sm


object Primitive:
  @Shallow def serializeBoolean(b: Boolean): String = String.valueOf(b)
  @Shallow def deserializeBoolean(sb: String): Boolean = sb.toBoolean

  @Shallow def serializeInt(i: Int): String = String.valueOf(i)
  @Shallow def deserializeInt(sb: String): Int = sb.toInt

  @Shallow def serializeLong(l: Long): String = String.valueOf(l)
  @Shallow def deserializeLong(sb: String): Long = sb.toLong

  @Shallow def serializeFloat(f: Float): String = String.valueOf(f)
  @Shallow def deserializeFloat(sb: String): Float = sb.toFloat
  
  @Shallow def serializeDouble(d: Double): String = String.valueOf(d)
  @Shallow def deserializeDouble(sb: String): Double = sb.toDouble

  @Shallow def serializeChar(c: Char): String = String.valueOf(c)
  @Shallow def deserializeChar(sb: String): Char = sb.head

  @Shallow def serializeString(s: String): String = String(s)
  @Shallow def deserializeString(sb: String): String = sb

import Primitive.*

/*
def trivialSerializer(i: Int): String = serializeInt(i)
def trivialDeserializer(sb: String): Int = deserializeInt(sb)

def trivialSerializer2(i: Int): String = trivialSerializer(i)
def trivialDeserializer2(sb: String): Int = trivialDeserializer(sb)

case class CaseClass(i: Int, d: Double, s: String)

def ccSerializer(cc: CaseClass): String =
  val sm = StringManipulator.empty
  val d = cc.d
  sm.push(trivialSerializer2(cc.i))
  sm.push(serializeDouble(d))
  sm.push(serializeString(cc.s))
  sm.string()

def ccDeserializer(str: String): CaseClass =
  val sm = StringManipulator.from(str)
  val s = deserializeString(sm.pop())
  val d = deserializeDouble(sm.pop())
  val i = trivialDeserializer2(sm.pop())
  new CaseClass(i, d, s)


case class CompCaseClass(cc1: CaseClass, cc2: CaseClass)

def cccSerializer(ccc: CompCaseClass): String =
  val sm = StringManipulator.empty
  val d = ccc.cc1
  sm.push(ccSerializer(d))
  sm.push(ccSerializer(ccc.cc2))
  sm.string()

def cccDeserializer(str: String): CompCaseClass =
  val sm = StringManipulator.from(str)
  val cc2 = ccDeserializer(sm.pop())
  val cc1 = ccDeserializer(sm.pop())
  new CompCaseClass(cc1, cc2)
*/

sealed trait Either
case class Left(x: Int) extends Either
case class Right(x: String) extends Either

def eSerializer(e: Either): String =
  val sm = StringManipulator.empty
  val ini = 0.0
  sm.push(serializeDouble(ini))
  e match
    case l: Left =>
      sm.push(serializeInt(l.x))
      sm.push("I")
    case r: Right =>
      sm.push(serializeString(r.x))
      sm.push("S")
  sm.string()
  
def eDeserializer(str: String): Either =
  val sm = StringManipulator.from(str)
  val x = sm.pop()
  x match
    case "I" =>
      val i = deserializeInt(sm.pop())
      val ini1 = deserializeDouble(sm.pop())
      val a = new Left(i)
      a
    case "S" =>
      val s = deserializeString(sm.pop())
      val ini2 = deserializeDouble(sm.pop())
      val b = new Right(s)
      b

// ---

object Main:
  def main(args: Array[String]): Unit =
    //val i = 0
    //println(i)
    //val m = trivialSerializer2(i)
    //println(m)
    //val o = trivialDeserializer2(m)
    //println(o)
    
    //val ii = new CaseClass(0, 0.0, "str")
    //val i = new CompCaseClass(ii, ii)
    //println(i)
    //val m = cccSerializer(i)
    //println(m)
    //val o = cccDeserializer(m)
    //println(o)

    val i = new Right("hello")
    println(i)
    val m = eSerializer(i)
    println(m)
    val o = eDeserializer(m)
    println(o)
  
