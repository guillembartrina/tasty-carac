package tastycarac.analyses.rules.experimental

import datalog.dsl.*
import datalog.dsl.__

import tastycarac.core.{Tasty, RuleSet, FactSet}
import tastycarac.analyses.facts


object Inv extends RuleSet:
  val dependencies: Set[FactSet | RuleSet] = Set(facts.experimental.Defs)
  
  val rules: Set[String] = Set(
    "Inverses", "Eq", "StrEq", "PartEq", "PhiEq",

    "InstrPrint",


    "LaterPushCall", "NextPushCall", "LastPushCall",
    "LaterPopCall", "NextPopCall", "FirstPopCall",

    "PhiArgs", "MatchCases"
  )

  def define(using Tasty): Unit =
    val F = facts.experimental.Defs

    val ser, deser, metha, methb = variable
    val bba, bbb, bbc, bbd = variable
    val insa, insb, insc, insd = variable
    val vara, varb, varc, vard = variable

    val cc, fld, i, j = variable

    // ---

    R.FirstPushCall(metha, bba, insa, vara) :- groupBy(F.PushCall(metha, bba, insb, vara, __, __), Seq(metha, bba, vara), AggOp.MIN(insb) -> insa)
    R.LaterPushCall(metha, bba, insa, insb, vara) :- (F.PushCall(metha, bba, insa, vara, __, __), F.PushCall(metha, bba, insb, vara, __, __), insa |<| insb)
    R.NextPushCall(metha, bba, insa, insb, vara) :- (F.PushCall(metha, bba, insa, vara, __, __), groupBy(R.LaterPushCall(metha, bba, insa, insc, vara), Seq(metha, bba, insa, vara), AggOp.MIN(insc) -> insb))
    R.NextPushCall(metha, bba, -1, insa, vara) :- (R.FirstPushCall(metha, bba, insa, vara))
    R.LastPushCall(metha, bba, insa, vara) :- groupBy(F.PushCall(metha, bba, insb, vara, __, __), Seq(metha, bba, vara), AggOp.MAX(insb) -> insa)
    R.LastPushCall(metha, bba, -1, vara) :- (F.StrCall(metha, __, __, vara, __), F.BB(metha, bba), F.Var(metha, vara), !F.PushCall(metha, bba, __, vara, __, __))

    R.LastPopCall(metha, bba, insa, vara) :- groupBy(F.PopCall(metha, bba, insb, vara, __), Seq(metha, bba, vara), AggOp.MAX(insb) -> insa)
    R.LaterPopCall(metha, bba, insa, insb, vara) :- (F.PopCall(metha, bba, insa, vara, __), F.PopCall(metha, bba, insb, vara, __), insa |<| insb)
    R.NextPopCall(metha, bba, insa, insb, vara) :- (F.PopCall(metha, bba, insb, vara, __), groupBy(R.LaterPopCall(metha, bba, insc, insb, vara), Seq(metha, bba, insb, vara), AggOp.MAX(insc) -> insa))
    R.NextPopCall(metha, bba, insa, -1, vara) :- (R.LastPopCall(metha, bba, insa, vara))
    R.FirstPopCall(metha, bba, insa, vara) :- groupBy(F.PopCall(metha, bba, insb, vara, __), Seq(metha, bba, vara), AggOp.MIN(insb) -> insa)
    R.FirstPopCall(metha, bba, -1, vara) :- (F.FromCall(metha, __, __, __, vara), F.BB(metha, bba), F.Var(metha, vara), !F.PopCall(metha, bba, __, vara, __))

    R.CaseClassFields(cc, i) :- groupBy(F.CaseClassField(cc, j, __), Seq(cc), AggOp.MAX(j) -> i)

    R.PhiArgs(metha, bba, insa, i) :- (F.PhiCall(metha, bba, insa), groupBy(F.ActualArg(metha, bba, insa, j, __), Seq(metha, bba, insa), AggOp.MAX(j) -> i))
    R.MatchCases(vara, i) :- groupBy(F.MatchCase(vara, j), Seq(vara), AggOp.COUNT(j) -> i)

    // ---

    R.Eq(vara, varb) :- F.Move(__, __, __, vara, varb)
    R.Eq(vara, varb) :- (R.Eq(vara, varc), R.Eq(varc, varb))

    /*
    R.Eq(vara, varb) :- (
      F.CandidateSerializer(ser, __),
      F.Call(metha, bba, insa, ser), F.ActualRet(metha, bba, insa, varb),
      F.FormalRet(ser, vara) // + Eq
    )

    R.Eq(vara, varb) :- (
      F.CandidateDeserializer(deser, __),
      F.Call(metha, bba, insa, deser), F.ActualArg(metha, bba, insa, 0, vara),
      F.FormalArg(deser, 0, varb) // + Eq
    )
    */

    R.Eq(vara, varb) :- (
      F.CandidateSerializer(ser, cc), F.CandidateDeserializer(deser, cc),
      F.FormalRet(ser, vara), F.FormalArg(deser, 0, varb)
    )

    R.Eq(vara, varb) :- (
      R.Inverses(ser, deser),
      F.Call(metha, bba, insa, ser), F.ActualArg(metha, bba, insa, 0, vara), F.ActualRet(metha, bba, insa, varc),
      R.Eq(varc, vard),
      F.Call(methb, bbb, insb, deser), F.ActualArg(methb, bbb, insb, 0, vard), F.ActualRet(methb, bbb, insb, varb)
    )

    R.Inverses(ser, deser) :- (
      F.CandidateSerializer(ser, __), F.CandidateDeserializer(deser, __),
      F.FormalArg(ser, 0, vara), F.FormalRet(deser, varb), R.Eq(vara, varb)
    )

    R.StrEq(metha, bba, insa, vara, methb, bbb, insb, varb) :- (
      R.Eq(varc, vard),
      F.StrCall(metha, bba, __, vara, varc),
      R.LastPushCall(metha, bba, insa, vara),
      F.FromCall(methb, bbb, __, vard, varb),
      R.FirstPopCall(methb, bbb, insb, varb)
    )

    R.StrEq(metha, bba, insa, vara, methb, bbb, insb, varb) :- (
      R.StrEq(metha, bba, insc, vara, methb, bbb, insd, varb),
      R.NextPushCall(metha, bba, insa, insc, vara),
      R.NextPopCall(methb, bbb, insd, insb, varb)
    )

    R.Eq(vara, varb) :- (
      R.StrEq(metha, bba, insa, varc, methb, bbb, insb, vard),
      F.PushCall(metha, bba, insa, varc, vara, __),
      F.PopCall(methb, bbb, insb, vard, varb)
    )

    R.StrEq(metha, bba, insa, vara, methb, bbb, insb, varb) :- (
      R.StrEq(metha, bbc, -1, vara, methb, bbb, insb, varb),
      F.SuccBB(metha, bba, bbc),
      R.LastPushCall(metha, bba, insa, vara),
      //insb |!=| -1
    )

    R.StrEq(metha, bba, insa, vara, methb, bbb, insb, varb) :- (
      R.StrEq(metha, bba, insa, vara, methb, bbd, -1, varb),
      F.SuccBB(methb, bbd, bbb),
      R.FirstPopCall(methb, bbb, insb, varb),
      //insa |!=| -1
    )

    // ---

    // Recursive call, assume inverses
    R.Eq(vara, varb) :- (
      F.Call(metha, bba, insa, metha), F.ActualArg(metha, bba, insa, 0, vara), F.ActualRet(metha, bba, insa, varc),
      R.Eq(varc, vard),
      F.Call(methb, bbb, insb, methb), F.ActualArg(methb, bbb, insb, 0, vard), F.ActualRet(methb, bbb, insb, varb)
    )

    // TODO: Handle mutually recursive serialization calls

    // ---
    
    (1 to 20).foreach(n => R.Nats(n) :- ())

    R.PartEq(vara, varb, cc, 0) :- (
      R.Eq(varc, vard),
      F.Load(metha, bba, insa, vara, fld, varc),
      F.CaseClassConstrCall(methb, bbb, insb, cc),
      F.CaseClassField(cc, 0, fld),
      F.ActualArg(methb, bbb, insb, 0, vard),
      F.ActualRet(methb, bbb, insb, varb)
    )

    R.PartEq(vara, varb, cc, i) :- (
      R.Eq(varc, vard),
      F.Load(metha, bba, insa, vara, fld, varc),
      F.CaseClassConstrCall(methb, bbb, insb, cc),
      F.CaseClassField(cc, i, fld),
      F.ActualArg(methb, bbb, insb, i, vard),
      F.ActualRet(methb, bbb, insb, varb),
      i |=| j+1, R.PartEq(vara, varb, cc, j)
    )

    R.Eq(vara, varb) :- (
      R.PartEq(vara, varb, cc, i),
      R.CaseClassFields(cc, i)
    )

    // --- Test

    R.PhiEq(vara, varb, metha, bba, insa, 0) :- (
      R.Eq(varc, vard),
      F.MatchCase(vara, varc),
      F.PhiCall(metha, bba, insa),
      F.ActualArg(metha, bba, insa, 0, vard),
      F.ActualRet(metha, bba, insa, varb)
    )

    R.PhiEq(vara, varb, metha, bba, insa, i) :- (
      R.Eq(varc, vard),
      F.MatchCase(vara, varc),
      F.PhiCall(metha, bba, insa),
      F.ActualArg(metha, bba, insa, i, vard),
      F.ActualRet(metha, bba, insa, varb),
      i |=| j+1, R.PhiEq(vara, varb, metha, bba, insa, j)
    )

    R.Eq(vara, varb) :- (
      R.PhiEq(vara, varb, metha, bba, insa, i),
      R.PhiArgs(metha, bba, insa, i),
      R.MatchCases(vara, j), i |=| j-1
    )

    // ---
    // Instructions

    R.InstrPrint(metha, bba, insa, "Call") :- F.Call(metha, bba, insa, __)
    R.InstrPrint(metha, bba, insa, "Move") :- F.Move(metha, bba, insa, __, __)
    R.InstrPrint(metha, bba, insa, "Load") :- F.Load(metha, bba, insa, __, __, __)
    R.InstrPrint(metha, bba, insa, "Literal") :- F.Literal(metha, bba, insa, __)
    R.InstrPrint(metha, bba, insa, "EmptyCall") :- F.EmptyCall(metha, bba, insa, __)
    R.InstrPrint(metha, bba, insa, "FromCall") :- F.FromCall(metha, bba, insa, __, __)
    R.InstrPrint(metha, bba, insa, "PushCall") :- F.PushCall(metha, bba, insa, __, __, __)
    R.InstrPrint(metha, bba, insa, "PopCall") :- F.PopCall(metha, bba, insa, __, __)
    R.InstrPrint(metha, bba, insa, "StrCall") :- F.StrCall(metha, bba, insa, __, __)
    R.InstrPrint(metha, bba, insa, "CaseClassConstrCall") :- F.CaseClassConstrCall(metha, bba, insa, __)
