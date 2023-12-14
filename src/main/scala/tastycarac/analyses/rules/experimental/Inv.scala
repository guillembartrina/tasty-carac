package tastycarac.analyses.rules.experimental

import datalog.dsl.*
import datalog.dsl.__

import tastycarac.core.{Tasty, RuleSet, FactSet}
import tastycarac.analyses.facts


object Inv extends RuleSet:
  val dependencies: Set[FactSet | RuleSet] = Set(facts.experimental.Defs)
  
  val rules: Set[String] = Set(
    "Inverses", "Eq", "StrEq", "PartEq",

    "InstrPrint"
  )

  def define(using Tasty): Unit =
    val F = facts.experimental.Defs

    val a, b, x, y, m, n, i, j, k, l, p, q, s, d = variable

    // ---

    R.FirstPushCall(m, i, x) :- groupBy(F.PushCall(m, j, x, __, __), Seq(m, x), AggOp.MIN(j) -> i)
    R.FinalPushCall(m, i, x) :- groupBy(F.PushCall(m, j, x, __, __), Seq(m, x), AggOp.MAX(j) -> i)
    R.AfterPushCall(m, i, j, x) :- (F.PushCall(m, i, x, __, __), F.PushCall(m, j, x, __, __), i |<| j)
    R.NextPushCall(m, i, j, x) :- (F.PushCall(m, i, x, __, __), groupBy(R.AfterPushCall(m, i, k, x), Seq(m, i, x), AggOp.MIN(k) -> j))
    R.NextPushCall(m, i, j, x) :- (R.FinalPushCall(m, i, x), F.StrCall(m, j, __, __))


    R.FirstPopCall(m, i, x) :- groupBy(F.PopCall(m, j, x, __), Seq(m, x), AggOp.MIN(j) -> i)
    R.FinalPopCall(m, i, x) :- groupBy(F.PopCall(m, j, x, __), Seq(m, x), AggOp.MAX(j) -> i)
    R.AfterPopCall(m, i, j, x) :- (F.PopCall(m, i, x, __), F.PopCall(m, j, x, __), i |<| j)
    R.NextPopCall(m, i, j, x) :- (F.PopCall(m, i, x, __), groupBy(R.AfterPopCall(m, i, k, x), Seq(m, i, x), AggOp.MIN(k) -> j))
    R.NextPopCall(m, i, j, x) :- (F.FromCall(m, i, __, __), R.FirstPopCall(m, j, x))

    R.CaseClassNumFields(a, i) :- groupBy(F.CaseClassField(a, j, __), Seq(a), AggOp.MAX(j) -> i)

    // ---

    R.Eq(a, b) :- F.Move(__, __, a, b)
    //R.StrEq(a, b) :- F.Move(__, __, a, b)
    R.Eq(a, b) :- (R.Eq(a, x), R.Eq(x, b))
    //R.StrEq(a, b) :- (R.StrEq(a, x), R.StrEq(x, b))
    //R.Eq(a, b) :- R.Eq(b, a)
    //R.StrEq(a, b) :- R.StrEq(b, a)

    R.Eq(a, b) :- (
      F.CandidateSerializer(s, __),
      F.Call(m, i, s), F.ActualRet(m, i, b),
      F.FormalRet(s, a) // + Eq
    )

    R.Eq(a, b) :- (
      F.CandidateDeserializer(d, __),
      F.Call(m, i, d), F.ActualArg(m, i, 0, a),
      F.FormalArg(d, 0, b) // + Eq
    )

    R.Eq(a, b) :- (
      R.Inverses(s, d),
      F.Call(m, i, s), F.ActualArg(m, i, 0, a), F.ActualRet(m, i, x),
      R.Eq(x, y),
      F.Call(n, j, d), F.ActualArg(n, j, 0, y), F.ActualRet(n, j, b)
    )

    R.Inverses(s, d) :- (
      F.CandidateSerializer(s, __), F.CandidateDeserializer(d, __),
      F.FormalArg(s, 0, a), F.FormalRet(d, b), R.Eq(a, b)
    )

    R.StrEq(a, m, i, b, n, j) :- (
      R.Eq(x, y),
      F.StrCall(m, i, a, x),
      F.FromCall(n, j, b, y)
    )

    R.StrEq(a, m, i, b, n, j) :- (
      R.StrEq(a, m, k, b, n, l),
      R.NextPushCall(m, i, k, a),
      R.NextPopCall(n, l, j, b)
    )

    R.Eq(a, b) :- (
      R.StrEq(x, m, i, y, n, j),
      F.PushCall(m, i, x, __, a),
      F.PopCall(n, j, y, b),
    )

    // ---
    
    (1 to 20).foreach(n => R.Nats(n) :- ())

    R.PartEq(l, a, b, 0) :- (
      F.CaseClassConstrCall(n, i, l),
      F.CaseClassField(l, 1, k),
      F.ActualArg(n, i, k, y),
      F.ActualRet(n, i, b),
      F.Load(m, __, x, a, k),
      R.Eq(x, y)
    )

    R.PartEq(l, a, b, p) :- (
      F.CaseClassConstrCall(n, i, l),
      F.CaseClassField(l, p, k),
      F.ActualArg(n, i, k, y),
      F.ActualRet(n, i, b),
      F.Load(m, __, x, a, k),
      R.Eq(x, y),
      p |=| q+1, R.PartEq(l, a, b, q)
    )

    R.Eq(a, b) :- (
      R.PartEq(l, a, b, k),
      R.CaseClassNumFields(l, k)
    )

    // ---
    // Instructions

    R.InstrPrint(m, i, "Call") :- F.Call(m, i, __)
    R.InstrPrint(m, i, "ActualArg") :- F.ActualArg(m, i, __, __)
    R.InstrPrint(m, i, "ActualRet") :- F.ActualRet(m, i, __)
    R.InstrPrint(m, i, "Move") :- F.Move(m, i, __, __)
    R.InstrPrint(m, i, "Load") :- F.Load(m, i, __, __, __)
    R.InstrPrint(m, i, "EmptyCall") :- F.EmptyCall(m, i, __)
    R.InstrPrint(m, i, "FromCall") :- F.FromCall(m, i, __, __)
    R.InstrPrint(m, i, "PushCall") :- F.PushCall(m, i, __, __, __)
    R.InstrPrint(m, i, "PopCall") :- F.PopCall(m, i, __, __)
    R.InstrPrint(m, i, "CaseClassConstrCall") :- F.CaseClassConstrCall(m, i, __)
