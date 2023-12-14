package tastycarac.analyses.rules.experimental

import datalog.dsl.*
import datalog.dsl.__

import tastycarac.core.{Tasty, RuleSet, FactSet}
import tastycarac.analyses.facts


object Inv extends RuleSet:
  val dependencies: Set[FactSet | RuleSet] = Set(facts.experimental.Defs)
  
  val rules: Set[String] = Set(
    "Inverses", "Eq", "StrEq",
    "FirstPackCall", "AfterPackCall", "NextPackCall",
    "FirstUnpackCall", "AfterUnpackCall", "NextUnpackCall",
    "InstrD",
    "Pack", "Unpack",
    "StrEq2"
  )

  def define(using Tasty): Unit =
    val F = facts.experimental.Defs

    val a, b, x, y, m, n, i, j, k, l, s, d = variable

    // ---

    R.FirstPackCall(m, i, x) :- groupBy(F.PackCall(m, j, x), Seq(m, x), AggOp.MIN(j) -> i)
    R.AfterPackCall(m, i, j, x) :- (F.PackCall(m, i, x), F.PackCall(m, j, x), i |<| j)
    R.NextPackCall(m, i, j, x) :- (F.PackCall(m, i, x), groupBy(R.AfterPackCall(m, i, k, x), Seq(m, i, x), AggOp.MIN(k) -> j))

    R.Pack(m, a, -1, y, j) :- (R.FirstPackCall(m, j, a), F.ActualArg(m, j, 0, b), F.Literal(m, __, b), F.ActualArg(m, j, 1, y))
    R.Pack(m, a, i, y, j) :- (
      R.Pack(m, a, __, __, i),
      R.NextPackCall(m, i, j, a),
      F.ActualArg(m, j, 0, b), F.Literal(m, __, b), F.ActualArg(m, j, 1, y)
    )

    R.FirstUnpackCall(m, i, x) :- groupBy(F.UnpackCall(m, j, x), Seq(m, x), AggOp.MIN(j) -> i)
    R.AfterUnpackCall(m, i, j, x) :- (F.UnpackCall(m, i, x), F.UnpackCall(m, j, x), i |<| j)
    R.NextUnpackCall(m, i, j, x) :- (F.UnpackCall(m, i, x), groupBy(R.AfterUnpackCall(m, i, k, x), Seq(m, i, x), AggOp.MIN(k) -> j))

    R.Unpack(m, a, -1, y, j) :- (R.FirstUnpackCall(m, j, a), F.ActualArg(m, j, 0, b), F.Literal(m, __, b), F.ActualRet(m, j, y))
    R.Unpack(m, a, i, y, j) :- (
      R.Unpack(m, a, __, __, i),
      R.NextUnpackCall(m, i, j, a),
      F.ActualArg(m, j, 0, b), F.Literal(m, __, b), F.ActualRet(m, j, y)
    )

    // ---


    //R.Eq(a, a) :- F.Var(__, a)
    //R.Eq(a, b) :- F.Move(__, __, a, b)
    //R.StrEq(a, b) :- F.Move(__, __, a, b)
    //R.Eq(a, b) :- R.Eq(b, a)
    //R.Eq(a, b) :- (R.Eq(a, x), R.Eq(x, b))

    //R.StrEq(a, b) :- R.StrEq(b, a)
    //R.StrEq(a, b) :- (R.StrEq(a, x), R.StrEq(x, b))

    /*
    R.StrEq(a, b) :- (
      F.CandidateSerializer(s, __), F.Call(m, i, s),
      F.ActualRet(m, i, x), F.FormalRet(s, a),
      R.StrEq(x, b)
    )

    R.StrEq(a, b) :- (
      F.CandidateDeserializer(d, __), F.Call(m, i, d),
      F.ActualArg(m, i, 0, x), F.FormalArg(d, 0, b),
      R.StrEq(a, x)
    )

    R.Eq(a, b) :- (
      R.Inverses(s, d),
      F.Call(m, i, s), F.ActualArg(m, i, 0, a), F.ActualRet(m, i, x),
      R.StrEq(x, y),
      F.Call(n, j, d), F.ActualArg(n, j, 0, y), F.ActualRet(n, j, b)
    )

    R.Inverses(s, d) :- (
      F.CandidateSerializer(s, __), F.CandidateDeserializer(d, __),
      F.FormalArg(s, 0, a), F.FormalRet(d, b), R.Eq(a, b)
    )

    // ---

    R.StrEq2(x, m, i, y, n, j) :- (
      F.CandidateSerializer(s, __), F.CandidateDeserializer(d, __),
      F.FormalRet(s, x), F.FormalArg(d, 0, y),
      R.StrEq(x, y),
      groupBy(R.Pack(x, m, __, __, k), Seq(x, m), AggOp.MAX(k) -> i),
      groupBy(R.Unpack(y, n, k, __, __), Seq(y, n), AggOp.MIN(k) -> j)
    )

    R.StrEq2(x, m, i, y, n, j) :- (
      R.StrEq2(x, m, k, y, n, l),
      R.Pack(x, m, i, __, k),
      R.Unpack(y, n, l, __, j)
    )

    R.StrEq(a, b) :- (
      R.StrEq2(x, m, k, y, n, l),
      R.Pack(x, m, __, a, k),
      R.Unpack(y, n, l, b, __)
    )

    // ---

    //R.PEq(a, b, 1) :- (
    //  F.Read(m, __, x, a, k),
    //  F.Constr(n, i, l),
    //  F.ActualArg(n, i, k, y),
    //  F.ActualRet(n, i, b),
    //  R.Eq(x, y)
    //)
    */

    // ---
    // Instructions

    R.InstrD(m, i, "Move") :- F.Move(m, i, __, __)
    R.InstrD(m, i, "Read") :- F.Read(m, i, __, __, __)
    R.InstrD(m, i, "Literal") :- F.Literal(m, i, __)
    R.InstrD(m, i, "ActualArg") :- F.ActualArg(m, i, __, __)
    R.InstrD(m, i, "ActualRet") :- F.ActualRet(m, i, __)
    R.InstrD(m, i, "InitCall") :- F.InitCall(m, i, __)
    R.InstrD(m, i, "PackCall") :- F.PackCall(m, i)
    R.InstrD(m, i, "UnpackCall") :- F.UnpackCall(m, i)
    R.InstrD(m, i, "Call") :- F.Call(m, i, __)
    R.InstrD(m, i, "CaseClassConstrCall") :- F.CaseClassConstrCall(m, i, __)
