
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

class Skip extends scala.annotation.StaticAnnotation
class Shallow extends scala.annotation.StaticAnnotation

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

@Skip object StringManipulator:
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

/*
def eSerializer(e: Either[Int, String]): String =
  val sm = StringManipulator.empty
  e match
    case Left(int) =>
      sm.push(serializeInt(int))
      sm.push("I")
    case Right(string) => sm.push(serializeString(string))
      sm.push("S")
  sm.string()
  
def eDeserializer(str: String): Either[Int, String] =
  val sm = StringManipulator.from(str)
  val x = sm.pop()
  x match
    case "I" => Left(deserializeInt(sm.pop()))
    case "S" => Right(deserializeString(sm.pop()))
*/

def main(): Unit =
  //val i = 0
  val i = new CaseClass(0, 0.0, "str")
  val j = new CompCaseClass(i, i)
  val m = cccSerializer(j)
  val o = cccDeserializer(m)
  println(j)
  println(o)
  
