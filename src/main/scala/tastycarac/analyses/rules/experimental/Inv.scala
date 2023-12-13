package tastycarac.analyses.rules.experimental

import datalog.dsl.__

import tastycarac.core.{Tasty, RuleSet, FactSet}
import tastycarac.analyses.facts


object Inv extends RuleSet:
  val dependencies: Set[FactSet | RuleSet] = Set(facts.experimental.Defs)
  
  val rules: Set[String] = Set(
    "Inverses", "Eq",
    "InstrD"
  )

  def define(using Tasty): Unit =
    val F = facts.experimental.Defs

    val a, b, x = variable

    R.Eq(a, b) :- F.Move(__, a, b)
    R.Eq(a, b) :- R.Eq(b, a)
    R.Eq(a, b) :- (R.Eq(a, x), R.Eq(x, b))


    val i = variable

    R.InstrD(i, "Move") :- F.Move(i, __, __)
    R.InstrD(i, "Read") :- F.Read(i, __, __, __)
    R.InstrD(i, "Literal") :- F.Literal(i, __)
    R.InstrD(i, "ActualArg") :- F.ActualArg(i, __, __)
    R.InstrD(i, "Constr") :- F.Constr(i, __)
    R.InstrD(i, "ActualRet") :- F.ActualRet(i, __)
    R.InstrD(i, "InitCall") :- F.InitCall(i, __)
    R.InstrD(i, "PlusCall") :- F.PlusCall(i)
    R.InstrD(i, "MinusCall") :- F.MinusCall(i)
    R.InstrD(i, "Call") :- F.Call(i, __)
