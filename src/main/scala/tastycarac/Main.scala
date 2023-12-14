package tastycarac

import java.nio.file.Paths

import datalog.dsl.*
import datalog.execution.*
import datalog.storage.*

import tastycarac.core.{Tasty, loadTasty}
import tastycarac.analyses.*


given Ordering[Seq[Constant]] with
  def compare(x: Seq[Constant], y: Seq[Constant]): Int =
    (x, y) match
      case ((a: Int) +: as, (b: Int) +: bs) => a.compareTo(b) match
        case 0 => compare(as, bs)
        case o => o
      case ((a: String) +: as, (b: String) +: bs) => a.compareTo(b) match
        case 0 => compare(as, bs)
        case o => o
      case _ => -1

@main def main() =
  val program = Program(SemiNaiveExecutionEngine(DefaultStorageManager()))

  //val explorer = program.loadTasty(List(path), Set(tastycarac.analyses.Explorer))

  //val dummy = program.loadTasty(List(path), Set(rules.PointsTo))
  //dummy.get{rules.PointsTo.LookUp}.solve().foreach(println(_))

  //val ptf = program.loadTasty(List(path), Set(facts.ClassInfo, facts.Heap, rules.PointsTo))
  //ptf.get{rules.PointsTo.Reachable}("main.Main$.main") :- ()

  //val tmp = ptf.get{facts.Heap.VCall}.solve().toList.sortBy(x => x(0).asInstanceOf[String])
  //tmp.foreach(x => println(x.mkString(", ")))

  /*
  val base, sig, invo, inMeth, heap, heapT, toMeth, thiss = program.variable()
  val tmp = program.relation("TEST")
  tmp(inMeth, base, heap, heapT, sig, toMeth, invo) :- (
    ptf.get{rules.PointsTo.Reachable}(inMeth),
    ptf.get{rules.PointsTo.VarPointsTo}(base, heap),
    ptf.get{facts.Heap.HeapType}(heap, heapT),
    ptf.get{rules.PointsTo.LookUp}(heapT, sig, toMeth),
    ptf.get{facts.Heap.VCall}(base, sig, invo, inMeth)
    //ptf.get{facts.Heap.HeapType}(toMeth, thiss)
  )
  tmp.solve().foreach(println)
  */

  /*
  val reach = ptf.get{rules.PointsTo.Reachable}.solve().toList.sortBy(x => x(0).asInstanceOf[String])
  reach.foreach(x => println(x.mkString(", ")))

  */
  //println("-----")

  //val vpt = ptf.get{rules.PointsTo.VarPointsTo}.solve().toList.sortBy(x => x(0).asInstanceOf[String])
  //vpt.foreach(x => println(x.mkString(", ")))

  simple(program)


def simple(program: Program): Unit = {

  val path = Paths.get("dummy/target/scala-3.3.0/classes")
  val simple = program.loadTasty(List(path), Set(facts.experimental.Defs, rules.experimental.Inv))

  // ---
  
  val tmp = simple.get{facts.experimental.Defs.CaseClassField}.solve().toList.asInstanceOf[List[Seq[Constant]]].sorted
  tmp.foreach(x => println(x.mkString(", ")))
  println("-----")

  //val tmp2 = simple.get{facts.experimental.Defs.Var}.solve().toList.asInstanceOf[List[Seq[String]]].sorted
  //tmp2.foreach(x => println(x.mkString(", ")))
  //println("-----")

  // ---

  val inverses = simple.get{rules.experimental.Inv.Inverses}

  inverses("simple.Primitive$.serializeBoolean", "simple.Primitive$.deserializeBoolean") :- ()

  inverses("simple.Primitive$.serializeInt", "simple.Primitive$.deserializeInt") :- ()
  inverses("simple.Primitive$.serializeLong", "simple.Primitive$.deserializeLong") :- ()

  inverses("simple.Primitive$.serializeFloat", "simple.Primitive$.deserializeFloat") :- ()
  inverses("simple.Primitive$.serializeDouble", "simple.Primitive$.deserializeDouble") :- ()

  inverses("simple.Primitive$.serializeChar", "simple.Primitive$.deserializeChar") :- ()
  inverses("simple.Primitive$.serializeString", "simple.Primitive$.deserializeString") :- ()


  val streq = simple.get{rules.experimental.Inv.Eq}

  streq("simple.simple$package$.main.m", "simple.simple$package$.main.m") :- ()

  /*
  val tmp21 = simple.get{rules.experimental.Inv.NextPushCall}.solve().toList.asInstanceOf[List[Seq[Constant]]].sorted
  tmp21.foreach(x => println(x.mkString(", ")))
  println("-----")

  val tmp22 = simple.get{rules.experimental.Inv.NextPopCall}.solve().toList.asInstanceOf[List[Seq[Constant]]].sorted
  tmp22.foreach(x => println(x.mkString(", ")))
  println("-----")
  */

  val tmp3 = simple.get{rules.experimental.Inv.Inverses}.solve().toList.asInstanceOf[List[Seq[Constant]]].sorted
  tmp3.foreach(x => println(x.mkString(", ")))
  println("-----")

}