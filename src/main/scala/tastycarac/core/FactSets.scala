package tastycarac.core

import scala.language.dynamics

import tastyquery.Contexts.Context
import tastyquery.Symbols.{Symbol, TermOrTypeSymbol}

import datalog.dsl.{Constant, Term, Atom, Relation}

import tasty.Symbols.SymbolTable


trait FactSet extends scala.Dynamic:
  private val id: String = getClass.getSimpleName.stripSuffix("$")
  private def patch(name: String): String = s"$id.$name"

  val facts: Set[String]
  def applyDynamic(fact: String)(terms: Term*)(using t: Tasty): Atom =
    val name = patch(fact)    
    if !facts.contains(fact) then throw Exception(s"Fact '$name' is not exposed")
    val patchedname = t.patch(name)
    if !t.program.ee.storageManager.ns.contains(patchedname) then t.program.relation[Constant](patchedname)
    t.program.namedRelation[Constant](patchedname)(terms*)
  def selectDynamic(fact: String)(using t: Tasty): Relation[Constant] =
    if !t.sets.contains(this) then throw Exception(s"FactSet '$id' is not available")
    val name = patch(fact)    
    if !facts.contains(fact) then throw Exception(s"Fact '$name' is not exposed")
    val patchedname = t.patch(name)
    if !t.program.ee.storageManager.ns.contains(patchedname) then t.program.relation[Constant](patchedname)
    t.program.namedRelation[Constant](patchedname)

  protected object F extends scala.Dynamic:
    def applyDynamic(fact: String)(terms: Term*)(using t: Tasty): Unit =
      val name = t.patch(patch(fact))     
      val rel = if t.program.ee.storageManager.ns.contains(name)
        then t.program.namedRelation[Constant](name)
        else t.program.relation[Constant](name)
      rel(terms*) :- ()
  protected def ST(using t: Tasty): SymbolTable = t.symbolTable

  def extract(toplevels: Set[TermOrTypeSymbol])(using Context)(using Tasty): Unit
