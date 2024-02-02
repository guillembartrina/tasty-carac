
package simple

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
  @Shallow def deserializeBoolean(s: String): Boolean = s.toBoolean

  @Shallow def serializeInt(i: Int): String = String.valueOf(i)
  @Shallow def deserializeInt(s: String): Int = s.toInt

  @Shallow def serializeLong(l: Long): String = String.valueOf(l)
  @Shallow def deserializeLong(s: String): Long = s.toLong

  @Shallow def serializeFloat(f: Float): String = String.valueOf(f)
  @Shallow def deserializeFloat(s: String): Float = s.toFloat
  
  @Shallow def serializeDouble(d: Double): String = String.valueOf(d)
  @Shallow def deserializeDouble(s: String): Double = s.toDouble

  @Shallow def serializeChar(c: Char): String = String.valueOf(c)
  @Shallow def deserializeChar(s: String): Char = s.head

  @Shallow def serializeString(s: String): String = String(s)
  @Shallow def deserializeString(s: String): String = s

  @Shallow def serializeList[A](la: List[A])(using ser: A => String): String =
    val sm = StringManipulator.empty
    for a <- la do
      sm.push(ser(a))
    sm.push(serializeInt(la.size))
    sm.string()

  @Shallow def deserializeList[A](s: String)(using deser: String => A): List[A] =
    val sm = StringManipulator.from(s)
    val size = deserializeInt(sm.pop())
    var la = List.empty[A]
    for _ <- 0 until size do
      val a = deser(sm.pop())
      la = a :: la
    la

import Primitive.*

/*
def trivialSerializer(i: Int): String = serializeInt(i)
def trivialDeserializer(str: String): Int = deserializeInt(str)

def trivialSerializer2(i: Int): String = trivialSerializer(i)
def trivialDeserializer2(str: String): Int = trivialDeserializer(str)

def semitrivialSerializer(li: List[Int]): String = serializeList[Int](li)(using serializeInt)
def semitrivialDeserializer(str: String): List[Int] = deserializeList[Int](str)(using deserializeInt)


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
*/

// ---

sealed trait JsonValue

case class JsonNull(_ignore: Int) extends JsonValue
case class JsonBoolean(value: Boolean) extends JsonValue
case class JsonNumber(value: Double) extends JsonValue
case class JsonString(value: String) extends JsonValue
case class JsonArray(values: List[JsonValue]) extends JsonValue
case class JsonObject(keys: List[String], values: List[JsonValue]) extends JsonValue

def serializeJsonValue(ser: JsonValue): String =
  val sm = StringManipulator.empty
  ser match
    case jn: JsonNull =>
      sm.push(serializeInt(jn._ignore))
      sm.push("null")
    case jb: JsonBoolean =>
      sm.push(serializeBoolean(jb.value))
      sm.push("boolean")
    case jn: JsonNumber =>
      sm.push(serializeDouble(jn.value))
      sm.push("number")
    case js: JsonString =>
      sm.push(serializeString(js.value))
      sm.push("string")
    case ja: JsonArray =>
      sm.push(serializeList[JsonValue](ja.values)(using serializeJsonValue))
      sm.push("array")
    case jo: JsonObject =>
      sm.push(serializeList[String](jo.keys)(using serializeString))
      sm.push(serializeList[JsonValue](jo.values)(using serializeJsonValue))
      sm.push("object")
  sm.string()

def deserializeJsonValue(deser: String): JsonValue =
  val sm = StringManipulator.from(deser)
  val k = sm.pop()
  k match
    case "null" =>
      new JsonNull(deserializeInt(sm.pop()))
    case "boolean" =>
      new JsonBoolean(deserializeBoolean(sm.pop()))
    case "number" =>
      new JsonNumber(deserializeDouble(sm.pop()))
    case "string" =>
      new JsonString(deserializeString(sm.pop()))
    case "array" =>
      new JsonArray(deserializeList[JsonValue](sm.pop())(using deserializeJsonValue))
    case "object" =>
      val values = deserializeList[JsonValue](sm.pop())(using deserializeJsonValue)
      val keys = deserializeList[String](sm.pop())(using deserializeString)
      new JsonObject(keys, values)

// ---

/*
sealed trait RecADT

case class Leaf(_filler: Int) extends RecADT
case class Node(a: RecADT, b: RecADT) extends RecADT

def raSerializer(e: RecADT): String =
  val sm = StringManipulator.empty
  e match
    case l: Leaf =>
      sm.push(serializeInt(l._filler))
      sm.push("L")
    case r: Node =>
      sm.push(raSerializer(r.a))
      sm.push(raSerializer(r.b))
      sm.push("N")
  sm.string()
  
def raDeserializer(str: String): RecADT =
  val sm = StringManipulator.from(str)
  val x = sm.pop()
  x match
    case "L" =>
      val i = deserializeInt(sm.pop())
      val a = new Leaf(i)
      a
    case "N" =>
      val b = raDeserializer(sm.pop())
      val a = raDeserializer(sm.pop())
      new Node(a, b)


sealed trait AList[A]
case class Nil[A](_filler: Int) extends AList[A]
case class Cons[A](h: A, t: AList[A]) extends AList[A]

def alistSerializer[A](l: AList[A])(using ser: A => String): String =
  val sm = StringManipulator.empty
  l match
    case n: Nil[A] =>
      sm.push(serializeInt(n._filler))
      sm.push("N")
    case c: Cons[A] =>
      sm.push(ser(c.h))
      sm.push(alistSerializer(c.t))
      sm.push("C")
  sm.string()
  
def alistDeserializer[A](str: String)(using deser: String => A): AList[A] =
  val sm = StringManipulator.from(str)
  val x = sm.pop()
  x match
    case "N" =>
      val i = deserializeInt(sm.pop())
      new Nil[A](i)
    case "C" =>
      val t = alistDeserializer(sm.pop())
      val h = deser(sm.pop())
      new Cons[A](h, t)
*/

// ---

object Main:
  def main(args: Array[String]): Unit =

    val i = JsonObject(List("peter"), List(JsonArray(List(JsonNull(0), JsonBoolean(true), JsonNumber(12.0), JsonString("str")))))
    println("INPUT:")
    println(i)
    val m = serializeJsonValue(i)
    println("SERIALIZED OBJECT: ")
    println(m)
    val o = deserializeJsonValue(m)
    println("OUTPUT:")
    println(o)
