package tastycarac.analyses.facts

import tastyquery.Contexts.Context
import tastyquery.Names.*
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*

import tastycarac.core.{Tasty, FactSet}

import tastycarac.core.tasty.IdGenerator
import tastycarac.core.tasty.Symbols.*


// REVISE: static things and objects
object Heap extends FactSet:
  val facts: Set[String] = Set(
    "Alloc", "Move", "Load", "Store", "HeapType",
    "FormalArg", "FormalReturn", "ActualArg", "ActualReturn", "ThisVar", "VCall",
    "Field", "SuperCall", "SCall",
    "Lambda"
  )

  private case class HeapContext(
    meth: Option[DefId],
    methThis: Option[ValId],
    temp: IdGenerator,
    instr: IdGenerator,
    alloc: IdGenerator  // ???
  ):
    def enterMethod(defId: DefId): HeapContext =
      copy(
        meth = Some(defId),
        temp = IdGenerator(defId + "/temp"),
        instr = IdGenerator(defId + "/instr"),
        alloc = IdGenerator(defId)
      )

    def enterClassMethod(defId: DefId): HeapContext =
      enterMethod(defId).copy(methThis = Some(thisValId(defId)))

    def safeMeth: DefId = meth.getOrElse(global)

  private object HeapContext:
    def init: HeapContext = HeapContext(None, None, IdGenerator("?/instr"), IdGenerator("?/temp"), IdGenerator("?"))


  def extract(toplevels: Set[TermOrTypeSymbol])(using Context)(using Tasty): Unit =
    toplevels.flatMap(_.tree).foreach(tl =>
      breakTree(tl)(using HeapContext.init)
    )

  // subtree that does not hang from class template
  private def breakTree(tree: Tree)(using HeapContext)(using Context)(using Tasty): Unit =
    tree match
      // non-class value definition (always has rhs)
      case ValDef(_, _, Some(rhs), symbol) => breakExpr(rhs, Some(ST.valId(symbol)))
      // non-class method definition
      case dd: DefDef => breakDefDef(dd)
      // class definition
      case cd: ClassDef => breakClassDef(cd)
      // term in statement position
      case t: TermTree => breakExpr(t)
      // other? -> TypeParams
      case _ => ()

  // class definition
  private def breakClassDef(classDef: ClassDef)(using hc: HeapContext)(using Context)(using Tasty): Unit = // TODO: Treat Module Classes specially? Related to THIS :O
    val symbol = classDef.symbol
    val classId = ST.classId(symbol)

    def processDefDef(defdef: DefDef): Unit =
      val defdefHC = hc.enterClassMethod(ST.defId(defdef.symbol))
      F.ThisVar(defdefHC.meth.get, defdefHC.methThis.get)
      breakDefDef(defdef)(using defdefHC)
      
    processDefDef(classDef.rhs.constr)
      
    val initHC = hc.enterClassMethod(ST.defId(classDef.rhs.constr.symbol))

    classDef.rhs.parents.foreach{
      case a: Apply =>
        val (fun, arglists) = unfoldCall(a)
        val instr = hc.instr.nextId
        arglists.zipWithIndex.foreach((arglist, i) =>
          arglist.zipWithIndex.foreach((arg, j) =>
            F.ActualArg(instr, s"l$i", s"a$j", exprAsVal(arg))
          )
        )
        F.VCall(initHC.methThis.get, ST.defId(fun.asInstanceOf[Select].symbol.asTerm), instr, initHC.meth.get)
      case tt: TypeTree =>
        classSymbol(tt.toType).foreach(_.tree.foreach(classdef =>
          F.VCall(initHC.methThis.get, classdef.rhs.constr.name.toString, hc.instr.nextId, initHC.meth.get)
        ))
      case b: Block => ???
    }

    classDef.rhs.body.foreach{
      // class method definition
      case dd: DefDef => processDefDef(dd)
      // class value/field definition
      case ValDef(name, _, rhs, symbol) =>  // Counts towards 'move' within <init>
        val valId = ST.valId(symbol)
        rhs match
          case None =>  // Constructor arguments
            classDef.rhs.constr.paramLists.collect{ case Left(a) => a }.flatten.find(_.name == name).foreach(param =>
              F.Move(valId, ST.valId(param.symbol))
              F.Field(valId, valId) 
            )
          case Some(expr) =>
            //F.Move(valId, exprAsVal(expr)(using initHC))
            breakExpr(expr, Some(valId))(using initHC)
            F.Field(valId, valId)
      // other statements
      case other => breakTree(other)(using initHC)
    }

  // (class or non-class) method definition
  private def breakDefDef(defdef: DefDef)(using hc: HeapContext)(using Context)(using Tasty): Unit =
    val defId = ST.defId(defdef.symbol)
    defdef.paramLists.zipWithIndex.foreach{
      case (Left(args), i) => args.zipWithIndex.foreach((arg, j) =>
        F.FormalArg(defId, s"l$i", s"a$j", ST.valId(arg.symbol))  // NTH: Rename param id
      )
      case _ => ()
    }
    defdef.rhs.foreach(body =>
      val ret = exprAsVal(body)(using hc.enterMethod(defId))
      F.FormalReturn(defId, ret)
    )

  // expr tree, whose result must be stored in 'to' (if not None)
  private def breakExpr(expr: TermTree, to: Option[ValId] = None)(using hc: HeapContext)(using Context)(using Tasty): Unit =
    expr match
      // Identifier, of val or def in same scope
      case id: Ident =>
        val symbol = id.symbol.asTerm  // symbol cannot be PackageSymbol?
        if symbol.isMethod then breakCall(id, to)
        else to.foreach(F.Move(_, ST.valId(symbol)))
      // Select, usually field selection but also call of parameterless method
      case s @ Select(base, _) =>
        val symbol = s.symbol.asTerm  // symbol cannot be PackageSymbol?
        if symbol.isMethod then breakCall(s, to)
        else to.foreach(F.Load(_, exprAsVal(base), ST.valId(s.symbol.asTerm), hc.safeMeth))
      // Method call
      case a: Apply => breakCall(a, to)
      case ta: TypeApply => breakCall(ta, to)
      // Assign, ignore 'to' because no heap
      case Assign(lhs, rhs) =>
        lhs match
          // Identifier, of val in same scope
          case id: Ident => breakExpr(rhs, Some(ST.valId(id.symbol.asTerm)))  // symbol cannot be PackageSymbol?
          // Select, must be a field selection
          case s @ Select(base, _) => F.Store(exprAsVal(base), ST.valId(s.symbol.asTerm), exprAsVal(rhs))
          case _ => ???  // Missing cases?
      case Block(stats, expr) =>
        stats.foreach(breakTree)
        breakExpr(expr, to)
      case If(cond, thenPart, elsePart) =>
        breakExpr(cond)
        breakExpr(thenPart, to)
        breakExpr(elsePart, to)
      case InlineIf(cond, thenPart, elsePart) =>
        breakExpr(cond)
        breakExpr(thenPart, to)
        breakExpr(elsePart, to)
      case Match(selector, cases) =>
        breakExpr(selector)
        cases.foreach(c =>  // No need to analyse pattern?
          c.guard.foreach(breakExpr(_))
          breakExpr(c.body, to)  
        )
      case InlineMatch(selector, cases) =>
        selector.foreach(breakExpr(_))
        cases.foreach(c =>  // No need to analyse pattern?
          c.guard.foreach(breakExpr(_))
          breakExpr(c.body, to)  
        )

      case Inlined(expr, _, bindings) =>
        bindings.foreach(breakTree)
        breakExpr(expr, to)

      // Lambda
      case Lambda(meth, _) =>  // TODO: Create virtual class to represent calls to lambda apply
        to.foreach(F.Lambda(_, ST.defId(meth.symbol.asTerm)))

        // Option 1: Define new class extension of FunctionX and link its apply method to DefDef + add function X to ClassInfo
        // Option 2: At call place, determine whether it is lamnda and if so set static call
      
      case NamedArg(_, arg) => breakExpr(arg, to)
      // Explicit return 
      case Return(expr, _) =>
        expr.foreach(e => F.FormalReturn(hc.safeMeth, exprAsVal(e)))
      // SeqLiteral, simply break all elements
      case SeqLiteral(elems, _) => elems.foreach(breakTree)
      // Throw, ignore 'to' because no heap
      case Throw(expr) => breakExpr(expr)
      // Try, consider all paths store to 'to'
      case Try(expr, cases, finalizer) =>
        breakExpr(expr, to)
        cases.foreach(c =>  // No need to analyse pattern?
          c.guard.foreach(breakExpr(_))
          breakExpr(c.body, to)  
        )
        finalizer.foreach(breakExpr(_))
      case Typed(expr, _) => breakExpr(expr, to)
      // While, ignore 'to' because no heap
      case While(cond, body) =>
        breakTree(cond)
        breakTree(body)
      // Literal, leaf
      case Literal(_) => ()
      // This, can it occur?
      case _: This => to.foreach(F.Move(_, hc.methThis.get))  // ??? [REMOVE?]
      // Implement?
      case _: SelectOuter => ???
      // Cannot occur
      case _: New => ???
      case _: Super => ???
  
  // Shortcut for simple expr (ident or this), otherwise create temp val and break expr
  private def exprAsVal(expr: TermTree)(using hc: HeapContext)(using Context)(using Tasty): ValId =
    expr match
      case id: Ident if !id.symbol.asTerm.isMethod => ST.valId(id.symbol.asTerm)  // symbol cannot be PackageSymbol?
      case _: This => hc.methThis.get
      case other =>
        val temp = hc.temp.nextId
        breakExpr(other, Some(temp))
        temp

  private def breakCall(call: TermTree, to: Option[ValId])(using ht: HeapContext)(using Context)(using Tasty): Unit =
    val (fun, arglists) = unfoldCall(call)
    val instr = ht.instr.nextId  // Identifies specific call

    fun match
      case s @ Select(Super(_, _), _) =>
        F.SuperCall(ST.defId(s.symbol.asTerm), instr, ht.safeMeth)
      case s @ Select(b @ New(tpt), _) =>
        val name = to.getOrElse(ht.temp.nextId)
        val classId = classSymbol(tpt.toType).map(ST.classId).getOrElse("<ERROR>")
        val site = s"new[$classId]{${ht.alloc.nextId}}"  // Identifies specific heap elem
        F.HeapType(site, classId)
        F.Alloc(name, site, ht.safeMeth)
        F.VCall(name, ST.defId(s.symbol.asTerm), instr, ht.safeMeth)
      case s @ Select(base, _) =>  // Handles 'this' case
        F.VCall(exprAsVal(base), ST.defId(s.symbol.asTerm), instr, ht.safeMeth)
      case id: Ident =>
        F.SCall(ST.defId(id.symbol.asTerm), instr, ht.safeMeth)
      case _ => ???  // Missing cases?

    fun match
      case Select(New(_), _) => ()
      case o => to.foreach(F.ActualReturn(instr, _))

    arglists.zipWithIndex.foreach((arglist, i) =>
      arglist.zipWithIndex.foreach((arg, j) =>
        F.ActualArg(instr, s"l$i", s"a$j", exprAsVal(arg))
      )  
    )

  private def unfoldCall(call: TermTree, acc: List[List[TermTree]] = Nil)(using HeapContext)(using Context)(using Tasty): (TermTree, List[List[TermTree]]) = call match
    case Apply(fun, args) => unfoldCall(fun, args :: acc)
    case TypeApply(fun, args) => unfoldCall(fun, Nil :: acc)
    case Block(stats, fun) =>  // If block is the outermost layer of the call it is treated elsewhere, but is ok
      stats.asInstanceOf[List[ValDef]].foreach(vd =>
        breakExpr(vd.rhs.get, Some(ST.valId(vd.symbol)))  
      )
      unfoldCall(fun, acc)      
    case _ => (call, acc)

  // Copied logic from tastyquery
  private def classSymbol(tpe: Type)(using Context): Option[ClassSymbol] =
    tpe match
      case ref: TypeRef => ref.optSymbol match
        case Some(cs: ClassSymbol) => Some(cs)
        case _ => ref.optAliasedType.flatMap(classSymbol)
      case ref: TermRef => ref.underlyingOrMethodic match
        case tpe: Type => classSymbol(tpe)
        case _ => None
      case tpe: TypeLambda => classSymbol(tpe.resultType)
      case tpe: TypeProxy => classSymbol(tpe.underlying)
      case _ => None
