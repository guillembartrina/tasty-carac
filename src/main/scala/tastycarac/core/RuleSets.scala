package tastycarac.core

import scala.language.dynamics

import datalog.dsl.{Constant, Variable, Term, Atom, Relation}


trait RuleSet extends scala.Dynamic:  // <-- Requires strict equality?
  private val id: String = getClass.getSimpleName.stripSuffix("$")
  private def patch(name: String): String = s"$id:$name"

  val dependencies: Set[FactSet | RuleSet]

  val rules: Set[String]
  def applyDynamic(rule: String)(terms: Term*)(using t: Tasty): Atom =
    val name = patch(rule)    
    if !rules.contains(rule) then throw Exception(s"Rule '$name' is not exposed")
    val patchedname = t.patch(name)
    if !t.program.ee.storageManager.ns.contains(patchedname) then t.program.relation[Constant](patchedname)
    t.program.namedRelation[Constant](patchedname)(terms*)
  def selectDynamic(rule: String)(using t: Tasty): Relation[Constant] =
    if !t.sets.contains(this) then throw Exception(s"RuleSet '$id' is not available")
    val name = patch(rule)    
    if !rules.contains(rule) then throw Exception(s"Rule '$name' is not exposed")
    val patchedname = t.patch(name)
    if !t.program.ee.storageManager.ns.contains(patchedname) then t.program.relation[Constant](patchedname)
    t.program.namedRelation[Constant](patchedname)

  protected object R extends scala.Dynamic:
    def applyDynamic(rule: String)(terms: Term*)(using t: Tasty): Atom =
      val name = t.patch(patch(rule))     
      val rel = if t.program.ee.storageManager.ns.contains(name)
        then t.program.namedRelation[Constant](name)
        else t.program.relation[Constant](name)
      rel(terms*)
  protected def variable(using t: Tasty): Variable = t.program.variable()

  def define(using Tasty): Unit
