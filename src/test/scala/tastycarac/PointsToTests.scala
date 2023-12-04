package tastycarac

import tastycarac.core.*
import tastycarac.analyses.rules.PointsTo


trait PointsToCommon:
  var tasty: Tasty
  def getReachable(): Set[String] = tasty.get{PointsTo.Reachable}.solve().map(_.head.asInstanceOf[String])
  def getVarPointsTo(svar: String): Set[String] =
    tasty.get{PointsTo.VarPointsTo}.solve().filter(x => x(0) == svar).map(x => x(1).asInstanceOf[String])


class PointsToArguments extends AnalysisTestSuite("pointsto/PointsToArguments.scala", Set(PointsTo)) with PointsToCommon:

  override def beforeAll(): Unit =
    super.beforeAll()
    tasty.get{PointsTo.Reachable}("pointstoarguments.Main$.main") :- ()

  test("reachability") {
    assert(
      Set(
        "pointstoarguments.Main$.main",
        "pointstoarguments.C1.<init>",
        "pointstoarguments.C1.test",
        "pointstoarguments.C2.<init>",
        "pointstoarguments.C2.test",
        "pointstoarguments.Main$.main.fun",
        "pointstoarguments.Pair.first",
        "pointstoarguments.Pair.second",
        // "pointstoarguments.Pair.swap",
        "pointstoarguments.TPair.meth1",
        "pointstoarguments.TPair.meth2",
      ).subsetOf(getReachable())
    )
  }

  test("allocation sites are correct") {
    val c1 = getVarPointsTo("pointstoarguments.Main$.main.c1")
    val c2 = getVarPointsTo("pointstoarguments.Main$.main.c2")

    val p1 = getVarPointsTo("pointstoarguments.Main$.main.p1")
    val p2 = getVarPointsTo("pointstoarguments.Main$.main.p2")
    val p3 = getVarPointsTo("pointstoarguments.Main$.main.p3")
    // val swap = getVarPointsTo("pointstoarguments.Main$.main.swap")

    val p1_v1 = getVarPointsTo("pointstoarguments.Main$.main.p1_v1")
    val p1_v2 = getVarPointsTo("pointstoarguments.Main$.main.p1_v2")

    val p2_v1 = getVarPointsTo("pointstoarguments.Main$.main.p2_v1")
    val p2_v2 = getVarPointsTo("pointstoarguments.Main$.main.p2_v2")

    val p3_v1 = getVarPointsTo("pointstoarguments.Main$.main.p3_v1")
    val p3_v2 = getVarPointsTo("pointstoarguments.Main$.main.p3_v2")
    val p3_t1 = getVarPointsTo("pointstoarguments.Main$.main.p3_t1")
    val p3_t2 = getVarPointsTo("pointstoarguments.Main$.main.p3_t2")

    // val swap_v1 = getVarPointsTo("pointstoarguments.Main$.main.swap_v1")
    // val swap_v2 = getVarPointsTo("pointstoarguments.Main$.main.swap_v2")

    assertEquals(p2_v1, c1)
    assertEquals(p2_v2, c2)

    assertEquals(p3_v1, c1)
    assertEquals(p3_v2, c2)

    assertNotEquals(p3_t1, p3_t2)

    // assertEquals(swap_v1, c2)
    // assertEquals(swap_v2, c1)
  }

  test("default arguments") {
    val default = getVarPointsTo("pointstoarguments.Main$.main.default")
    val res1 = getVarPointsTo("pointstoarguments.Main$.main.res1")
    val res2 = getVarPointsTo("pointstoarguments.Main$.main.res2")
    val res3 = getVarPointsTo("pointstoarguments.Main$.main.res3")
    val c11 = getVarPointsTo("pointstoarguments.Main$.main.c11")

    assertEquals(res1, default)
    assertEquals(res2, default)
    assertEquals(res3, c11) // make sure default is not used
  }


class PointsToConstructors extends AnalysisTestSuite("pointsto/PointsToConstructors.scala", Set(PointsTo)) with PointsToCommon:

  override def beforeAll(): Unit =
    super.beforeAll()
    tasty.get{PointsTo.Reachable}("pointstoconstructors.Main$.main") :- ()

  test("reachability") {
    assert(
      Set(
        "pointstoconstructors.T.<init>",
        "pointstoconstructors.A.<init>",
        "pointstoconstructors.B.<init>",
        "pointstoconstructors.Main$.main"
      ).subsetOf(getReachable())
    )
  }

  test("allocation sites are correct") {
    val o1 = getVarPointsTo("pointstoconstructors.Main$.main.o1")
    val o2 = getVarPointsTo("pointstoconstructors.Main$.main.o2")

    // println("LookUp")
    // for (f <- program.namedRelation("LookUp").get()) println(f)

    // println("FieldValDef")
    // for (f <- program.namedRelation("FieldValDef").get()) println(f)

    // println("PointsTo")
    // for (f <- program.namedRelation("VarPointsTo").get()) println(f)

    // println("VCall")
    // for (f <- program.namedRelation("VCall").get()) println(f)

    // println("ActualArg")
    // for (f <- program.namedRelation("ActualArg").get()) println(f)

    // println("FormalArg")
    // for (f <- program.namedRelation("FormalArg").get()) println(f)

    // println("CallGraph")
    // for (f <- program.namedRelation("CallGraph").get()) println(f)

    // println("InterProcAssign")
    // for (f <- program.namedRelation("InterProcAssign").get()) println(f)

    val b_argB = getVarPointsTo("pointstoconstructors.Main$.main.b_argB")
    val b_argA = getVarPointsTo("pointstoconstructors.Main$.main.b_argA")
    val b_get = getVarPointsTo("pointstoconstructors.Main$.main.b_get")
    // val b_argA0 = getVarPointsTo("pointstoconstructors.Main$.main.b_argA0")
    // val b_argA1 = getVarPointsTo("pointstoconstructors.Main$.main.b_argA1")
    // println(b_argA1)
    
    assertEquals(b_argA, o1)
    assertEquals(b_argB, o2)
    assertEquals(b_get, o2)
    // assertEquals(b_argA1, o1)
  }


class PointsToFields extends AnalysisTestSuite("pointsto/PointsToFields.scala", Set(PointsTo)) with PointsToCommon:

  override def beforeAll(): Unit =
    super.beforeAll()
    tasty.get{PointsTo.Reachable}("pointstofields.Main$.main") :- ()

  test("reachability") {
    assert(
      Set(
        "pointstofields.Main$.main",
        "pointstofields.MyObject.<init>",
        "pointstofields.MyObject.fun1",
        "pointstofields.MyObject.fun2",
        "pointstofields.A.<init>",
      ).subsetOf(getReachable())
    )
  }

  test("allocation sites are correct") {
    val initX = getVarPointsTo("pointstofields.Main$.main.initX")
    
    val a1 = getVarPointsTo("pointstofields.Main$.main.a1")
    val a2 = getVarPointsTo("pointstofields.Main$.main.a2")
    
    val x1 = getVarPointsTo("pointstofields.Main$.main.x1")
    val x2 = getVarPointsTo("pointstofields.Main$.main.x2")

    assert(initX.subsetOf(x1))
    assert(initX.subsetOf(x2))
    assert(a1.subsetOf(x1))
    assert(a2.subsetOf(x2))
    assertEquals(x1.intersect(x2).size, 1)
  }


class PointsToFun extends AnalysisTestSuite("pointsto/PointsToFun.scala", Set(PointsTo)) with PointsToCommon:

  override def beforeAll(): Unit =
    super.beforeAll()
    tasty.get{PointsTo.Reachable}("pointstofun.Main$.main") :- ()

  test("reachability") {
    assert(
      Set(
        "pointstofun.Main$.main",
        "pointstofun.PointsToFun.fun1",
        "pointstofun.PointsToFun.fun2",
        "pointstofun.PointsToFun.id"
      ).subsetOf(getReachable())
    )
  }

  test("allocation sites are correct") {
    val a1 = getVarPointsTo("pointstofun.PointsToFun.fun1.a1")
    val a2 = getVarPointsTo("pointstofun.PointsToFun.fun2.a2")
    val b1 = getVarPointsTo("pointstofun.PointsToFun.fun1.b1")
    val b2 = getVarPointsTo("pointstofun.PointsToFun.fun2.b2")

    assertEquals(a1.size, 1)
    assertEquals(a2.size, 1)
    assertEquals(a1.intersect(a2), Set.empty)
    assert(a1.intersect(b1).nonEmpty)
    assert(a2.intersect(b2).nonEmpty)
  }


class PointsToInheritance extends AnalysisTestSuite("pointsto/PointsToInheritance.scala", Set(PointsTo)) with PointsToCommon:

  override def beforeAll(): Unit =
    super.beforeAll()
    tasty.get{PointsTo.Reachable}("pointstoinheritance.Main$.main") :- ()

  test("reachability") {
    assert(
      Set(
        "pointstoinheritance.Main$.main",
        "pointstoinheritance.A.test",
        "pointstoinheritance.A.privateTest",
        "pointstoinheritance.A.withPrivateCall",
        "pointstoinheritance.A.withNonPrivateCall",
        "pointstoinheritance.B.test",        
      ).subsetOf(getReachable())
    )

    assert(!getReachable().contains("pointstoinheritance.B.privateTest"))
  }

  test("allocation sites are correct") {
    val a_instance = getVarPointsTo("pointstoinheritance.Main$.main.a_instance")
    val b_instance = getVarPointsTo("pointstoinheritance.Main$.main.b_instance")

    val a_x = getVarPointsTo("pointstoinheritance.Main$.main.a_x")
    val a_xx = getVarPointsTo("pointstoinheritance.Main$.main.a_xx")
    val a_t = getVarPointsTo("pointstoinheritance.Main$.main.a_t")
    val a_t2 = getVarPointsTo("pointstoinheritance.Main$.main.a_t2")
    val a_pc = getVarPointsTo("pointstoinheritance.Main$.main.a_pc")
    val a_npc = getVarPointsTo("pointstoinheritance.Main$.main.a_npc")

    assertEquals(a_x, a_instance)
    assertEquals(a_xx, a_instance)
    assertEquals(a_t, a_instance)
    assertEquals(a_t2, a_instance)
    assertEquals(a_pc, a_instance)
    assertEquals(a_npc, a_instance)

    val b_x = getVarPointsTo("pointstoinheritance.Main$.main.b_x")
    val b_xx = getVarPointsTo("pointstoinheritance.Main$.main.b_xx")
    val b_t = getVarPointsTo("pointstoinheritance.Main$.main.b_t")
    val b_t2 = getVarPointsTo("pointstoinheritance.Main$.main.b_t2")
    val b_pc = getVarPointsTo("pointstoinheritance.Main$.main.b_pc")
    val b_npc = getVarPointsTo("pointstoinheritance.Main$.main.b_npc")
    val b_sup = getVarPointsTo("pointstoinheritance.Main$.main.b_sup")
    val b_traitVal = getVarPointsTo("pointstoinheritance.Main$.main.b_traitVal")

    assertEquals(b_x, b_instance)
    assertEquals(b_xx, a_instance)
    assertEquals(b_t, b_instance)
    assertEquals(b_t2, b_instance)
    assertEquals(b_pc, a_instance)
    assertEquals(b_npc, b_instance)
    assertEquals(b_sup, a_instance)
    
    val c_x = getVarPointsTo("pointstoinheritance.Main$.main.c_x")
    val c_xx = getVarPointsTo("pointstoinheritance.Main$.main.c_xx")
    val c_t = getVarPointsTo("pointstoinheritance.Main$.main.c_t")
    val c_pc = getVarPointsTo("pointstoinheritance.Main$.main.c_pc")
    val c_npc = getVarPointsTo("pointstoinheritance.Main$.main.c_npc")
    
    assertEquals(c_x, a_instance)
    assertEquals(c_xx, a_instance)
    assertEquals(c_t, a_instance)
    assertEquals(c_pc, a_instance)
    assertEquals(c_npc, a_instance)

    val d_x = getVarPointsTo("pointstoinheritance.Main$.main.d_x")
    val d_xx = getVarPointsTo("pointstoinheritance.Main$.main.d_xx")
    val d_t = getVarPointsTo("pointstoinheritance.Main$.main.d_t")
    val d_pc = getVarPointsTo("pointstoinheritance.Main$.main.d_pc")
    val d_npc = getVarPointsTo("pointstoinheritance.Main$.main.d_npc")
    val d_sup = getVarPointsTo("pointstoinheritance.Main$.main.d_sup")
    val d_traitVal = getVarPointsTo("pointstoinheritance.Main$.main.d_traitVal")

    assertEquals(d_x, b_instance)
    assertEquals(d_xx, a_instance)
    assertEquals(d_t, b_instance)
    assertEquals(d_pc, a_instance)
    assertEquals(d_npc, b_instance)
    assertEquals(d_sup, a_instance)
    assertEquals(d_traitVal, b_instance)
  }


class PointsToNested extends AnalysisTestSuite("pointsto/PointsToNested.scala", Set(PointsTo)) with PointsToCommon:

  override def beforeAll(): Unit =
    super.beforeAll()
    tasty.get{PointsTo.Reachable}("pointstonested.Main$.main") :- ()

  test("reachability") {
    assert(
      Set(
        "pointstonested.Main$.main",
        "pointstonested.Main$.main.fun",
        "pointstonested.Main$.main.fun.fun",
        "pointstonested.Main$.main.b.fun",
      ).subsetOf(getReachable())
    )
  }

  test("allocation sites are correct") {
    val a = getVarPointsTo("pointstonested.Main$.main.a")
    val b = getVarPointsTo("pointstonested.Main$.main.b")
    val c = getVarPointsTo("pointstonested.Main$.main.c")

    assertEquals(a, c)
    assertNotEquals(a, b)
  }

import tastycarac.analyses.facts.Heap

class PointsToLambda extends AnalysisTestSuite("pointsto/PointsToLambda.scala", Set(Heap, PointsTo)) with PointsToCommon:

  override def beforeAll(): Unit =
    super.beforeAll()
    tasty.get{PointsTo.Reachable}("pointstolambda.Main$.main") :- ()

  test("reachability") {
    assert(
      Set(
        "pointstolambda.Main$.main",
        "pointstolambda.Main$.nonlamb",
        "pointstolambda.A.<init>"
      ).subsetOf(getReachable())
    )
  }

  test("pointsto2") {

    val x = getVarPointsTo("pointstolambda.Main$.main.x")
    val y = getVarPointsTo("pointstolambda.Main$.main.y")
    val y2 = getVarPointsTo("pointstolambda.Main$.main.y2")
    val z = getVarPointsTo("pointstolambda.Main$.main.z")

    assertEquals(x, y)
    assertEquals(x, y2)
    assertEquals(x, z)

    //tasty.get{Heap.Lambda}.solve().toList.sortBy(_(1).asInstanceOf[String]).foreach(println)

    //val initX = getVarPointsTo("pointstofields.Main$.main.initX")
    
    //val a1 = getVarPointsTo("pointstofields.Main$.main.a1")
    //val a2 = getVarPointsTo("pointstofields.Main$.main.a2")
    
    //val x1 = getVarPointsTo("pointstofields.Main$.main.x1")
    //val x2 = getVarPointsTo("pointstofields.Main$.main.x2")

    //assert(initX.subsetOf(x1))
    //assert(initX.subsetOf(x2))
    //assert(a1.subsetOf(x1))
    //assert(a2.subsetOf(x2))
    //assertEquals(x1.intersect(x2).size, 1)
  }