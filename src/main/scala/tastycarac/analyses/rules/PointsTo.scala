package tastycarac.analyses.rules

import datalog.dsl.__

import tastycarac.core.{Tasty, RuleSet, FactSet}
import tastycarac.analyses.facts


object PointsTo extends RuleSet:
  val dependencies: Set[FactSet | RuleSet] = Set(facts.ClassInfo, facts.Heap)
  
  val rules: Set[String] = Set(
    "LookUp", "InterProcAssign",
    "Reachable", "CallGraph",
    "VarPointsTo", "FldPointsTo"
  )

  def define(using Tasty): Unit =
    val C = facts.ClassInfo
    val F = facts.Heap

    val varr, heap, meth, to, from, base, baseH, fld, ref = variable
    val toMeth, thiss, thisFrom, invo, sig, inMeth, heapT, m, n, actualFld = variable
    val classA, classB, classC, sigA, sigB, sigC = variable

    R.VarPointsTo(varr, heap) :- (R.Reachable(meth), F.Alloc(varr, heap, meth))
    R.VarPointsTo(to, heap) :- (F.Move(to, from), R.VarPointsTo(from, heap))
    R.FldPointsTo(baseH, fld, heap) :- (F.Store(base, fld, from), R.VarPointsTo(from, heap), R.VarPointsTo(base, baseH))
    R.VarPointsTo(to, heap) :- (F.Load(to, base, fld, inMeth), R.VarPointsTo(base, baseH), R.FldPointsTo(baseH, fld, heap))

    R.Reachable(toMeth) :-
      (F.VCall(base, sig, invo, inMeth), R.Reachable(inMeth),
        R.VarPointsTo(base, heap),
        F.HeapType(heap, heapT), R.LookUp(heapT, sig, toMeth),
        F.ThisVar(toMeth, thiss))

    R.VarPointsTo(thiss, heap) :-
      (F.VCall(base, sig, invo, inMeth), R.Reachable(inMeth),
        R.VarPointsTo(base, heap),
        F.HeapType(heap, heapT), R.LookUp(heapT, sig, toMeth),
        F.ThisVar(toMeth, thiss))

    R.CallGraph(invo, toMeth) :-
      (F.VCall(base, sig, invo, inMeth), R.Reachable(inMeth),
        R.VarPointsTo(base, heap),
        F.HeapType(heap, heapT), R.LookUp(heapT, sig, toMeth),
        F.ThisVar(toMeth, thiss))

    // rules for dynamic val
    R.Reachable(toMeth ) :-
        (F.Load(to, base, sig, inMeth), R.Reachable(inMeth),
        R.VarPointsTo(base, heap),
        F.HeapType(heap, heapT), R.LookUp(heapT, sig, toMeth),
        F.ThisVar(toMeth, thiss),
        F.FormalReturn(toMeth, from))

    R.VarPointsTo(thiss, heap) :-
        (F.Load(to, base, sig, inMeth), R.Reachable(inMeth),
        R.VarPointsTo(base, heap),
        F.HeapType(heap, heapT), R.LookUp(heapT, sig, toMeth),
        F.ThisVar(toMeth, thiss),
        F.FormalReturn(toMeth, from))

    R.InterProcAssign(to, from) :-
        (F.Load(to, base, sig, inMeth), R.Reachable(inMeth),
        R.VarPointsTo(base, heap),
        F.HeapType(heap, heapT), R.LookUp(heapT, sig, toMeth),
        F.ThisVar(toMeth, thiss),
        F.FormalReturn(toMeth, from))

    R.InterProcAssign(to, from) :- (R.CallGraph(invo, meth), F.FormalArg(meth, m, n, to), F.ActualArg(invo, m, n, from))

    R.InterProcAssign(to, from) :- (R.CallGraph(invo, meth), F.FormalReturn(meth, from), F.ActualReturn(invo, to))

    R.VarPointsTo(to, heap) :- (R.InterProcAssign(to, from), R.VarPointsTo(from, heap))

    R.Reachable(toMeth) :- (F.SCall(toMeth, invo, inMeth), R.Reachable(inMeth))

    R.CallGraph(invo, toMeth) :- (F.SCall(toMeth, invo, inMeth), R.Reachable(inMeth))

    R.LookUp(classC, sig, meth) :- C.Defines(classC, sig, meth)
    R.LookUp(classC, sigA, sigB) :- (R.LookUp(classB, sigA, sigB), !C.Defines(classC, sigA, __), C.Extends(classC, classB))
    C.Defines(classC, sigA, sigC) :- (C.Defines(classC, sigB, sigC), C.Defines(classB, sigA, sigB))

    R.Reachable(toMeth) :-
      (F.SuperCall(toMeth, invo, inMeth), R.Reachable(inMeth),
      F.ThisVar(inMeth, thisFrom), R.VarPointsTo(thisFrom, heap),
      F.ThisVar(toMeth, thiss))

    R.VarPointsTo(thiss, heap) :-
      (F.SuperCall(toMeth, invo, inMeth), R.Reachable(inMeth),
      F.ThisVar(inMeth, thisFrom), R.VarPointsTo(thisFrom, heap),
      F.ThisVar(toMeth, thiss))

    R.CallGraph(invo, toMeth) :-
      (F.SuperCall(toMeth, invo, inMeth), R.Reachable(inMeth),
      F.ThisVar(inMeth, thisFrom), R.VarPointsTo(thisFrom, heap),
      F.ThisVar(toMeth, thiss))

    R.VarPointsTo(to, heap) :-
      (F.Load(to, base, fld, inMeth), R.VarPointsTo(base, baseH),
      F.HeapType(baseH, heapT), R.LookUp(heapT, fld, actualFld),
      F.Field(actualFld, from),
      R.VarPointsTo(from, heap))
    
    F.SCall(toMeth, invo, inMeth) :- (F.Lambda(varr, toMeth), F.VCall(varr, __, invo, inMeth))
    