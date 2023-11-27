package tastycarac.core

import scala.language.dynamics

import datalog.dsl.{Constant, Variable, Term, Atom, Relation}


trait RuleSet extends scala.Dynamic:  // <-- Require strict equality?
  private val id: String = getClass.getSimpleName.stripSuffix("$")
  private def patch(name: String): String = s"${id}:${name}"

  val dependencies: Set[FactSet | RuleSet]

  val rules: Set[String]
  def applyDynamic(rule: String)(terms: Term*)(using t: Tasty): Atom =
    val name = patch(rule)    
    if !rules.contains(rule) then throw Exception(s"Rule '${name}' is not exposed")
    t.program.namedRelation[Constant](t.patch(name))(terms*)
  def selectDynamic(rule: String)(using t: Tasty): Relation[Constant] =
    val name = patch(rule)    
    if !rules.contains(rule) then throw Exception(s"Rule '${name}' is not exposed")
    t.program.namedRelation[Constant](t.patch(name))

  protected object Rresolver extends scala.Dynamic:
    def applyDynamic(rule: String)(terms: Term*)(using t: Tasty): Atom =
      val name = t.patch(patch(rule))     
      val rel = if t.program.ee.storageManager.ns.contains(name)
        then t.program.namedRelation[Constant](name)
        else t.program.relation[Constant](name)
      rel(terms*)
  protected val R = Rresolver
  protected def variable(using t: Tasty): Variable = t.program.variable()

  def define(using Tasty): Unit


// ---

object TestRS extends RuleSet:
  val dependencies: Set[FactSet | RuleSet] = Set(TestFS)

  val rules: Set[String] = Set("TestR")

  def define(using Tasty): Unit =
    val v = variable
    R.TestR(v) :- TestFS.TestF(v)
    R.TestInnerR(v) :- ()

object TestRSX extends RuleSet:
  val dependencies: Set[FactSet | RuleSet] = Set(TestRS)

  val rules: Set[String] = Set("TestR")

  def define(using Tasty): Unit =
    val v = variable
    R.TestR(v) :- TestFS.TestF(v)
    R.TestInnerR(v) :- ()


case class TestRS2(main: Option[String]) extends RuleSet:
  val dependencies: Set[FactSet | RuleSet] = Set(TestFS)
  val rules: Set[String] = Set("CustomR")

  def define(using Tasty): Unit =
    val v = variable
    R.TestInnerR(v) :- (TestFS.TestF(v))
    R.CustomR(main.getOrElse("NONE")) :- R.TestInner(12)
