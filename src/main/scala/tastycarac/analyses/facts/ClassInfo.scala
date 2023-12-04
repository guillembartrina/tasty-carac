package tastycarac.analyses.facts

import tastyquery.Contexts.Context
import tastyquery.Names.*
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*

import tastycarac.core.{Tasty, FactSet}
import tastycarac.core.tasty.Symbols.*
import tastycarac.core.tasty.Traversals.*


object ClassInfo extends FactSet:
  val facts: Set[String] = Set(
    "Extends", "Defines"
  )

  def extract(toplevels: Set[TermOrTypeSymbol])(using Context)(using Tasty): Unit =   
    toplevels.flatMap(_.tree.toList).foreach(t =>
      traverse(t){
        case ClassDef(_, _, symbol) =>
          symbol.parentClasses.foreach(psymbol => F.Extends(ST.classId(symbol), ST.classId(psymbol)))
          symbol.declarations.collect{ case ts: TermSymbol => ts }.foreach(decl =>
            F.Defines(
              ST.classId(symbol),
              if decl.isMethod then ST.defId(decl) else ST.valId(decl),
              if decl.isMethod then ST.defId(decl) else ST.valId(decl)
            )
            decl.allOverriddenSymbols.foreach(odecl =>
              F.Defines(
                ST.classId(symbol),
                if odecl.isMethod then ST.defId(odecl) else ST.valId(odecl),
                if decl.isMethod then ST.defId(decl) else ST.valId(decl)
              )
            )
          )
          // NotDefines facts needed? Generate LooKUp facts directly?
        case _ => ()
      }  
    )
