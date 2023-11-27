package tastycarac.core

import scala.language.dynamics

import tastyquery.Contexts.Context
import tastyquery.Symbols.PackageSymbol

import datalog.dsl.{Constant, Term, Atom, Relation}


trait FactSet extends scala.Dynamic:
  private val id: String = getClass.getSimpleName.stripSuffix("$")
  private def patch(name: String): String = s"${id}.${name}"

  val facts: Set[String]
  def applyDynamic(fact: String)(terms: Term*)(using t: Tasty): Atom =
    val name = patch(fact)    
    if !facts.contains(fact) then throw Exception(s"Fact '${name}' is not exposed")
    t.program.namedRelation[Constant](t.patch(name))(terms*)
  def selectDynamic(fact: String)(using t: Tasty): Relation[Constant] =
    val name = patch(fact)    
    if !facts.contains(fact) then throw Exception(s"Fact '${name}' is not exposed")
    t.program.namedRelation[Constant](t.patch(name))

  protected object Fresolver extends scala.Dynamic:
    def applyDynamic(fact: String)(terms: Term*)(using t: Tasty): Unit =
      val name = t.patch(patch(fact))     
      val rel = if t.program.ee.storageManager.ns.contains(name)
        then t.program.namedRelation[Constant](name)
        else t.program.relation[Constant](name)
      rel(terms*) :- ()
  protected val F = Fresolver

  def extract(root: PackageSymbol)(using Context)(using Tasty): Unit


// ---

object TestFS extends FactSet:

  val facts: Set[String] = Set("TestF")

  def extract(root: PackageSymbol)(using Context)(using Tasty): Unit =
    F.TestF(12)
    F.TestInnerF("a", 12)
