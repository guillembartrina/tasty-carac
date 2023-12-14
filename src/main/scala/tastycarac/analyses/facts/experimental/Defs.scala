package tastycarac.analyses.facts.experimental

import tastyquery.Contexts.Context
import tastyquery.Names.*
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*

import tastycarac.core.{Tasty, FactSet}

import tastycarac.core.tasty.{Counter, IdGenerator}
import tastycarac.core.tasty.Symbols.*


object Defs extends FactSet:
  val facts: Set[String] = Set(
    "Method", "FormalArg", "FormalRet",
    "Call", "ActualArg", "ActualRet",
    "PackCall", "UnpackCall", "InitCall",
    "Instr", "Var",
    "Move", "Read",
    "CandidateSerializer", "CandidateDeserializer",
    "Literal",
    "CaseClassConstrCall", "CaseClass", "CaseClassField"
  )

  private case class DefsContext(
    meth: Option[DefId],
    instr: Counter,
    temp: IdGenerator
  ):
    def enterMethod(defId: DefId): DefsContext =
      copy(
        meth = Some(defId),
        instr = Counter(),
        temp = IdGenerator(defId + "/temp")
      )

    def safeMeth: DefId = meth.getOrElse(global)

  private object DefsContext:
    def init: DefsContext = DefsContext(None, Counter(), IdGenerator("?/temp"))

  
  private def _stringClass(using ctx: Context): ClassSymbol =
    ctx.findTopLevelClass("simple._String")

  private def _stringPlusMethod(using ctx: Context): TermSymbol =
    _stringClass.getNonOverloadedDecl(termName("+=")).get

  private def _stringMinusMethod(using ctx: Context): TermSymbol =
    _stringClass.getNonOverloadedDecl(termName("=-")).get

  private def _stringEmptyMethod(using ctx: Context): TermSymbol =
    ctx.findTopLevelClass("simple._String").companionClass.get.getNonOverloadedDecl(termName("empty")).get

  private def newInstr(using dc: DefsContext)(using Context)(using Tasty): Int =
    val instr = dc.instr.next
    F.Instr(dc.safeMeth, instr)
    instr

  private def newTemp(using dc: DefsContext)(using Context)(using Tasty): String =
    val temp = dc.temp.nextId
    F.Var(dc.safeMeth, temp)
    temp

  def extract(toplevels: Set[TermOrTypeSymbol])(using Context)(using Tasty): Unit =
    toplevels.flatMap(_.tree).foreach(tl =>
      breakTree(tl)(using DefsContext.init)
    )

  // subtree that does not hang from class template
  private def breakTree(tree: Tree)(using dc: DefsContext)(using Context)(using Tasty): Unit =
    tree match
      // non-class value definition (always has rhs)
      case ValDef(_, _, Some(rhs), symbol) if dc.meth.isDefined =>
        F.Var(dc.safeMeth, ST.valId(symbol))
        breakExpr(rhs, Some(ST.valId(symbol)))
      // non-class method definition
      case dd: DefDef if dc.meth.isDefined => breakDefDef(dd)
      // class definition
      case cd: ClassDef if cd.symbol.isModuleClass =>
        cd.rhs.body.foreach{
          // class method definition
          case dd: DefDef if !dd.symbol.isSynthetic => breakDefDef(dd)
          // other
          case _ => ()
        }
      
      case cc: ClassDef if cc.symbol.isCaseClass =>
        val symbol = cc.symbol
        F.CaseClass(ST.classId(symbol))

        val pnames = symbol.companionClass.get.getNonOverloadedDecl(termName("apply")).get
          .declaredType.asInstanceOf[MethodType].paramNames.map(_.toString())

        pnames.foreach(F.CaseClassField(ST.classId(symbol), _))

      // term in statement position
      case t: TermTree if dc.meth.isDefined => breakExpr(t)
      // other? -> TypeParams
      case _ => ()

  // (class or non-class) method definition
  private def breakDefDef(defdef: DefDef)(using dc: DefsContext)(using Context)(using Tasty): Unit =
    val defId = ST.defId(defdef.symbol)
    F.Method(defId)
    
    if defdef.symbol.annotations.isEmpty then
      // Only a single parameter list for now
      defdef.paramLists(0).left.get.zipWithIndex.foreach((arg, i) =>
        F.FormalArg(defId, i, ST.valId(arg.symbol))
      )
      defdef.rhs.foreach(body =>
        val ret = breakExpr(body)(using dc.enterMethod(defId))
        F.FormalRet(defId, ret)
      )

      val methTpe = defdef.symbol.declaredType.asInstanceOf[MethodType]    
      if methTpe.paramNames.size == 1 then
        val parTpe = methTpe.paramTypes(0)
        val resTpe = methTpe.resultType.requireType
        if resTpe.isSameType(_stringClass.staticRef) then
          classSymbol(parTpe).foreach(cs => F.CandidateSerializer(defId, ST.classId(cs)))
        if parTpe.isSameType(_stringClass.staticRef) then
          classSymbol(resTpe).foreach(cs => F.CandidateDeserializer(defId, ST.classId(cs)))

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
            F.Move(dc.safeMeth, newInstr, value, ST.valId(symbol))
            value
      // Select, usually field selection but also call of parameterless method
      case s @ Select(base, name) =>
        val symbol = s.symbol.asTerm  // symbol cannot be PackageSymbol?
        if symbol.isMethod then breakCall(s, to)
        else
          val ret = breakExpr(base)
          val temp = to.getOrElse(newTemp)
          F.Read(dc.safeMeth, newInstr, temp, ret, name.toString())
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
        dc.temp.nextId
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
        F.Literal(dc.safeMeth, newInstr, temp)
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
            F.ActualArg(dc.safeMeth, instr, n, valId)
          )

          F.CaseClassConstrCall(dc.safeMeth, instr, ST.classId(cs))
          F.ActualRet(dc.safeMeth, instr, temp)
        temp
      case s @ Select(base, _) =>
        val temp = to.getOrElse(newTemp)
        if s.symbol == _stringEmptyMethod then 
          F.InitCall(dc.safeMeth, instr, temp)
        else
          val ret = breakExpr(base)
          valIds.zipWithIndex.foreach((valId, i) =>
            F.ActualArg(dc.safeMeth, instr, i, valId)
          )
          if s.symbol.asTerm == _stringPlusMethod then
            F.PackCall(dc.safeMeth, instr, ret)
          if s.symbol.asTerm == _stringMinusMethod then
            F.UnpackCall(dc.safeMeth, instr, ret)
            F.ActualRet(dc.safeMeth, instr, temp)
        temp
      case id: Ident =>
        val temp = to.getOrElse(newTemp)
        valIds.zipWithIndex.foreach((valId, i) =>
          F.ActualArg(dc.safeMeth, instr, i, valId)
        )
        F.Call(dc.safeMeth, instr, ST.defId(id.symbol.asTerm))
        F.ActualRet(dc.safeMeth, instr, temp)
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
