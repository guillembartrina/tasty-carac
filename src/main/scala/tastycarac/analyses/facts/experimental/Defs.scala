package tastycarac.analyses.facts.experimental

import tastyquery.Contexts.Context
import tastyquery.Names.*
import tastyquery.Symbols.*
import tastyquery.Annotations.*
import tastyquery.Trees.*
import tastyquery.Types.*

import tastycarac.core.{Tasty, FactSet}

import tastycarac.core.tasty.{Counter, IdGenerator}
import tastycarac.core.tasty.Symbols.*


object Defs extends FactSet:
  val facts: Set[String] = Set(
    "CandidateSerializer", "CandidateDeserializer",

    "Method", "FormalArg", "FormalRet",

    "Call", "ActualArg", "ActualRet",

    "Instr", "Var",

    "Move", "Load", "Literal",  // ??

    "EmptyCall", "FromCall", "PushCall", "PopCall", "StrCall",

    "CaseClassConstrCall", "CaseClass", "CaseClassField"
  )


  private class DefsContext(meth: DefId, bb: Counter, instr: Counter, temp: IdGenerator):
    def getMeth: DefId = meth
    def nextBb: Int = bb.next
    def nextInstr: Int = instr.next
    def nextTemp: String = s"$meth/${temp.nextId}"
  
  private object DefsContext:    
    def enter(defId: DefId): DefsContext =
      DefsContext(
        meth = defId,
        bb = Counter(),
        instr = Counter(),
        temp = IdGenerator("temp")
      )


  private object DefsDefs:
    def SkipClass(using ctx: Context): ClassSymbol =
      ctx.findTopLevelClass("simple.Skip")

    def ShallowClass(using ctx: Context): ClassSymbol =
      ctx.findTopLevelClass("simple.Shallow")

    def StringClass(using ctx: Context): ClassSymbol =
      ctx.defn.StringClass

    def StringManipulatorClass(using ctx: Context): ClassSymbol =
      ctx.findTopLevelClass("simple.StringManipulator")

    def StringManipulatorPushMethod(using Context): TermSymbol =
      StringManipulatorClass.getNonOverloadedDecl(termName("push")).get

    def StringManipulatorPopMethod(using Context): TermSymbol =
      StringManipulatorClass.getNonOverloadedDecl(termName("pop")).get

    def StringManipulatorStringMethod(using Context): TermSymbol =
      StringManipulatorClass.getNonOverloadedDecl(termName("string")).get

    def StringManipulatorEmpty(using Context): TermSymbol =
      StringManipulatorClass.companionClass.get.getNonOverloadedDecl(termName("empty")).get

    def StringManipulatorFrom(using Context): TermSymbol =
      StringManipulatorClass.companionClass.get.getNonOverloadedDecl(termName("from")).get

  private def skip(symbol: Symbol)(using Context): Boolean =
    symbol.annotations.map(_.symbol).contains(DefsDefs.SkipClass)

  private def shallow(symbol: Symbol)(using Context): Boolean =
    symbol.annotations.map(_.symbol).contains(DefsDefs.ShallowClass)


  private def newInstr(using dc: DefsContext)(using Context)(using Tasty): Int =
    val instr = dc.nextInstr
    F.Instr(dc.getMeth, instr)
    instr

  private def newTemp(using dc: DefsContext)(using Context)(using Tasty): String =
    val temp = dc.nextTemp
    F.Var(dc.getMeth, temp)
    temp


  def extract(toplevels: Set[TermOrTypeSymbol])(using Context)(using Tasty): Unit =
    def rec(tree: Tree): Unit = tree match
      // class definition
      case cd: ClassDef if !skip(cd.symbol) =>
        val symbol = cd.symbol
        if symbol.isModuleClass then  // object
          cd.rhs.body.foreach{
            // method definition
            case dd: DefDef if !dd.symbol.isSynthetic => rec(dd)
            // other
            case _ => ()
          }
        else if symbol.isCaseClass then  // case class
          F.CaseClass(ST.classId(symbol))
          val parnames = symbol.companionClass.get.getNonOverloadedDecl(nme.m_apply).get
            .declaredType.asInstanceOf[MethodType].paramNames.map(_.toString())
          parnames.zipWithIndex.foreach((n, i) => F.CaseClassField(ST.classId(symbol), i, n))
        else ()
      // non-class method definition
      case dd: DefDef if !skip(dd.symbol) => breakDefDef(dd)
      // other
      case _ => ()

    toplevels.flatMap(_.tree).foreach(rec)


  // (class or non-class) method definition
  private def breakDefDef(defdef: DefDef)(using Context)(using Tasty): Unit =
    val symbol = defdef.symbol
    val defId = ST.defId(symbol)
    F.Method(defId)
    
    if !shallow(symbol) then
      defdef.rhs.foreach(body =>
        val ret = breakExpr(body)(using DefsContext.enter(defId))
        F.FormalRet(defId, ret)
      )
      
      defdef.paramLists match
        case Left(args) :: Nil =>  // only a single parameter list for now
          args.zipWithIndex.foreach((arg, i) =>
            F.FormalArg(defId, i, ST.valId(arg.symbol))
          )

          val methTpe = symbol.declaredType.asInstanceOf[MethodType]
          if methTpe.paramNames.size == 1 then
            val parTpe = methTpe.paramTypes(0)
            val resTpe = methTpe.resultType.requireType
            if resTpe.isSameType(DefsDefs.StringClass.staticRef) then
              classSymbol(parTpe).foreach(cs => F.CandidateSerializer(defId, ST.classId(cs)))
            if parTpe.isSameType(DefsDefs.StringClass.staticRef) then
              classSymbol(resTpe).foreach(cs => F.CandidateDeserializer(defId, ST.classId(cs)))

        case _ => ()


  // subtree that does not hang from class template
  private def breakTree(tree: Tree)(using dc: DefsContext)(using Context)(using Tasty): Unit = tree match
    // non-class value definition (always has rhs)
    case ValDef(_, _, Some(rhs), symbol) =>
      F.Var(dc.getMeth, ST.valId(symbol))
      breakExpr(rhs, Some(ST.valId(symbol)))
    // term in statement position
    case t: TermTree => breakExpr(t)
    // other
    case _ => ()


  // expr tree, whose result must be stored in 'to' (if not None)
  private def breakExpr(expr: TermTree, to: Option[ValId] = None)(using dc: DefsContext)(using Context)(using Tasty): ValId =
    expr match
      // Identifier, of val or def in same scope
      case id: Ident =>
        val symbol = id.symbol.asTerm  // symbol cannot be PackageSymbol?
        if symbol.isMethod then breakCall(id, to)
        else to match
          case None => ST.valId(symbol)
          case Some(value) =>
            F.Move(dc.getMeth, newInstr, value, ST.valId(symbol))
            value
      // Select, usually field selection but also call of parameterless method
      case s @ Select(base, name) =>
        val symbol = s.symbol.asTerm  // symbol cannot be PackageSymbol?
        if symbol.isMethod then breakCall(s, to)
        else
          val ret = breakExpr(base)
          val temp = to.getOrElse(newTemp)
          F.Load(dc.getMeth, newInstr, temp, ret, name.toString())
          temp
      // Method call
      case a: Apply => breakCall(a, to)
      case ta: TypeApply => ???
      // Assign, ignore 'to' because no heap
      case Assign(lhs, rhs) =>
        lhs match
          // Identifier, of val in same scope
          case id: Ident => breakExpr(rhs, Some(ST.valId(id.symbol.asTerm)))
          // Select, must be a field selection
          case s @ Select(base, _) => ???  // No store?
          case _ => ???  // Missing cases?
        dc.nextTemp
      case Block(stats, expr) =>
        stats.foreach(breakTree)
        breakExpr(expr, to)
      case If(cond, thenPart, elsePart) => ???
      case InlineIf(cond, thenPart, elsePart) => ???
      case Match(selector, cases) => ???
      case InlineMatch(selector, cases) => ???
      case Inlined(expr, _, bindings) => ???
      // Lambda
      case Lambda(meth, _) => ???
      case NamedArg(_, arg) => ???
      // Explicit return 
      case Return(expr, _) => ???
      // SeqLiteral, simply break all elements
      case SeqLiteral(elems, _) => ???
      // Throw, ignore 'to' because no heap
      case Throw(expr) => ???
      // Try, consider all paths store to 'to'
      case Try(expr, cases, finalizer) => ???
      case Typed(expr, _) => breakExpr(expr, to)
      // While, ignore 'to' because no heap
      case While(cond, body) => ???
      // Literal, leaf
      case Literal(ctant) =>
        val temp = to.getOrElse(newTemp)
        F.Literal(dc.getMeth, newInstr, temp)  // Add info about literal?
        temp
      // This, can it occur?
      case _: This => ??? //to.foreach(F.Move(_, hc.methThis.get))  // ??? [REMOVE?]
      // Implement?
      case _: SelectOuter => ???
      // Cannot occur
      case _: New => ???
      case _: Super => ???


  private def breakCall(call: TermTree, to: Option[ValId])(using dc: DefsContext)(using Context)(using Tasty): ValId =
    val (fun, args) = call match
      case Apply(fun, args) => (fun, args)
      case _ => (call, Nil)
    val valIds = args.map(breakExpr(_))
    val instr = newInstr

    fun match
      case s @ Select(b @ New(tpt), _) =>
        val temp = to.getOrElse(newTemp)
        val cs = classSymbol(tpt.toType).get
        if cs.isCaseClass then
          val pnames = cs.companionClass.get.getNonOverloadedDecl(termName("apply")).get
            .declaredType.asInstanceOf[MethodType].paramNames.map(_.toString())

          valIds.zip(pnames).foreach((valId, n) =>
            F.ActualArg(dc.getMeth, instr, n, valId)
          )

          F.CaseClassConstrCall(dc.getMeth, instr, ST.classId(cs))
          F.ActualRet(dc.getMeth, instr, temp)
        temp
      case s @ Select(base, _) =>
        val temp = to.getOrElse(newTemp)
        if s.symbol == DefsDefs.StringManipulatorEmpty then 
          F.EmptyCall(dc.getMeth, instr, temp)
        if s.symbol == DefsDefs.StringManipulatorFrom then 
          F.FromCall(dc.getMeth, instr, temp, valIds(0))
        else
          val ret = breakExpr(base)
          if s.symbol.asTerm == DefsDefs.StringManipulatorPushMethod then
            F.PushCall(dc.getMeth, instr, ret, temp, valIds(0))
          if s.symbol.asTerm == DefsDefs.StringManipulatorPopMethod then
            F.PopCall(dc.getMeth, instr, ret, temp)
          if s.symbol.asTerm == DefsDefs.StringManipulatorStringMethod then
            F.StrCall(dc.getMeth, instr, ret, temp)
        temp
      case id: Ident =>
        val temp = to.getOrElse(newTemp)
        valIds.zipWithIndex.foreach((valId, i) =>
          F.ActualArg(dc.getMeth, instr, i, valId)
        )
        F.Call(dc.getMeth, instr, ST.defId(id.symbol.asTerm))
        F.ActualRet(dc.getMeth, instr, temp)
        temp
      case _ => ???  // Missing cases?


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
