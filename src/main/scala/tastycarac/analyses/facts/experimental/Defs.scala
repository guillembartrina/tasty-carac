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

    "Move", "Load", "Literal",  // ??

    "EmptyCall", "FromCall", "PushCall", "PopCall", "StrCall",

    "CaseClass", "Extends", "CaseClassField",
    "CaseClassConstrCall",

    "MatchCase", "PhiCall",

    "Var", "BB", "Instr", // Not necessary?
    "SuccBB"
  )


  private class DefsContext(meth: DefId, temp: IdGenerator, bb: Counter, instr: Counter):
    def getMeth: DefId = meth
    def nextTemp: String = s"$meth/${temp.nextId}"
    def nextBB: Int = bb.next
    def nextInstr: Int = instr.next

    def nextChild: (Int, DefsContext) =
      val nbb = nextBB
      val ndc = DefsContext(meth, temp, bb, Counter())
      (nbb, ndc)

    def nextThis: Int =
      val nbb = nextBB
      instr.reset
      nbb

  private object DefsContext:    
    def enter(defId: DefId): DefsContext =
      DefsContext(
        meth = defId,
        temp = IdGenerator("temp"),
        bb = { val tmp = Counter(); tmp.next; tmp },
        instr = Counter()
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

  private def newTemp(using dc: DefsContext)(using Context)(using Tasty): String =
    val temp = dc.nextTemp
    F.Var(dc.getMeth, temp)
    temp

  private def newChild(using dc: DefsContext)(using Context)(using Tasty): (Int, DefsContext) =
    val res = dc.nextChild
    F.BB(dc.getMeth, res._1)
    res

  private def newThis(using dc: DefsContext)(using Context)(using Tasty): Int =
    val res = dc.nextThis
    F.BB(dc.getMeth, res)
    res

  private def newInstr(bb: Int)(using dc: DefsContext)(using Context)(using Tasty): Int =
    val instr = dc.nextInstr
    F.Instr(dc.getMeth, bb, instr)
    instr


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
          symbol.parentClasses.foreach(psymbol => F.Extends(ST.classId(symbol), ST.classId(psymbol)))

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
        val (_, ret) = breakExpr(body, 0)(using DefsContext.enter(defId))
        F.BB(defId, 0)
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
  private def breakTree(tree: Tree, bb: Int)(using dc: DefsContext)(using Context)(using Tasty): Int = tree match
    // non-class value definition (always has rhs)
    case ValDef(_, _, Some(rhs), symbol) =>
      F.Var(dc.getMeth, ST.valId(symbol))
      val (nbb, _) = breakExpr(rhs, bb, Some(ST.valId(symbol)))
      nbb
    // term in statement position
    case t: TermTree =>
      val (nbb, _) = breakExpr(t, bb)
      nbb
    // other
    case _ => bb


  // expr tree, whose result must be stored in 'to' (if not None)
  private def breakExpr(expr: TermTree, bb: Int, to: Option[ValId] = None)(using dc: DefsContext)(using Context)(using Tasty): (Int, ValId) =
    expr match
      // Identifier, of val or def in same scope
      case id: Ident =>
        val symbol = id.symbol.asTerm  // symbol cannot be PackageSymbol?
        if symbol.isMethod then breakCall(id, bb, to)
        else to match
          case None => (bb, ST.valId(symbol))
          case Some(value) =>
            F.Move(dc.getMeth, bb, newInstr(bb), value, ST.valId(symbol))
            (bb, value)
      // Select, usually field selection but also call of parameterless method
      case s @ Select(base, name) =>
        val symbol = s.symbol.asTerm  // symbol cannot be PackageSymbol?
        if symbol.isMethod then breakCall(s, bb, to)
        else
          val (nbb, ret) = breakExpr(base, bb)
          val temp = to.getOrElse(newTemp)
          F.Load(dc.getMeth, bb, newInstr(bb), ret, name.toString(), temp)
          (nbb, temp)
      // Method call
      case a: Apply => breakCall(a, bb, to)
      case ta: TypeApply => ???
      // Assign, ignore 'to' because no heap
      case Assign(lhs, rhs) =>
        val (nbb, _) = lhs match
          // Identifier, of val in same scope
          case id: Ident => breakExpr(rhs, bb, Some(ST.valId(id.symbol.asTerm)))
          // Select, must be a field selection
          case s @ Select(base, _) => ???  // No store?
          case _ => ???  // Missing cases?
        (nbb, dc.nextTemp)
      case Block(stats, expr) =>
        val nbb = stats.foldLeft(bb)((acc, st) => breakTree(st, acc))
        breakExpr(expr, nbb, to)
      case If(cond, thenPart, elsePart) => ???
      case InlineIf(cond, thenPart, elsePart) => ???
      case Match(selector, cases) =>
        val (mbb, ret) = breakExpr(selector, bb)
        
        // Redo considering guards?
        val ndcs = List.fill(cases.size)(newChild)
        ndcs.foreach((cbb, _) => F.SuccBB(dc.getMeth, mbb, cbb))
        val rets = cases.zip(ndcs).map((c, cbb) =>
          c.pattern match
            case Bind(_, TypeTest(WildcardPattern(_), _), symbol) =>
              F.MatchCase(ret, ST.valId(symbol))
            case _ => ()
          breakExpr(c.body, cbb._1)(using cbb._2)
        )

        val nbb = newThis
        val instr = newInstr(nbb)
        val temp = to.getOrElse(newTemp)
        rets.zipWithIndex.foreach((cret, i) =>
          F.SuccBB(dc.getMeth, cret._1, nbb)
          F.ActualArg(dc.getMeth, nbb, instr, i, cret._2)
        )
        F.PhiCall(dc.getMeth, nbb, instr)        
        F.ActualRet(dc.getMeth, nbb, instr, temp)
        (nbb, temp)
      case InlineMatch(selector, cases) => ???
      case Inlined(expr, caller, bindings) => ???
      // Lambda
      case Lambda(meth, tpt) => ???
      case NamedArg(name, arg) => ???
      // Explicit return 
      case Return(expr, from) => ???
      // SeqLiteral, simply break all elements
      case SeqLiteral(elems, _) => ???
      // Throw, ignore 'to' because no heap
      case Throw(expr) => ???
      // Try, consider all paths store to 'to'
      case Try(expr, cases, finalizer) => ???
      case Typed(expr, tpt) => breakExpr(expr, bb, to)
      // While, ignore 'to' because no heap
      case While(cond, body) => ???
      // Literal, leaf
      case Literal(ctant) =>
        val temp = to.getOrElse(newTemp)
        F.Literal(dc.getMeth, bb, newInstr(bb), temp)  // Add info about literal?
        (bb, temp)
      // This, can it occur?
      case _: This => ??? //to.foreach(F.Move(_, hc.methThis.get))  // ??? [REMOVE?]
      // Implement?
      case _: SelectOuter => ???
      // Cannot occur
      case _: New => ???
      case _: Super => ???


  private def breakCall(call: TermTree, bb: Int, to: Option[ValId])(using dc: DefsContext)(using Context)(using Tasty): (Int, ValId) =
    val (fun, args) = call match
      case Apply(fun, args) => (fun, args)
      case _ => (call, Nil)

    fun match
      case s @ Select(b @ New(tpt), _) =>
        val (nbb, valIds) = args.foldLeft((bb, List.empty[ValId]))((acc, arg) =>
          val (tbb, ret) = breakExpr(arg, acc._1)
          (tbb, acc._2 :+ ret)
        )
        val instr = newInstr(nbb)

        val temp = to.getOrElse(newTemp)
        val cs = classSymbol(tpt.toType).get
        if cs.isCaseClass then
          //val pnames = cs.companionClass.get.getNonOverloadedDecl(termName("apply")).get
          //.declaredType.asInstanceOf[MethodType].paramNames.map(_.toString())

          valIds.zipWithIndex.foreach((valId, i) =>
            F.ActualArg(dc.getMeth, nbb, instr, i, valId)
          )

          F.CaseClassConstrCall(dc.getMeth, nbb, instr, ST.classId(cs))
          F.ActualRet(dc.getMeth, nbb, instr, temp)
        (nbb, temp)
      case s @ Select(base, _) =>
        val temp = to.getOrElse(newTemp)
        if s.symbol == DefsDefs.StringManipulatorEmpty then
          val instr = newInstr(bb)
          F.EmptyCall(dc.getMeth, bb, instr, temp)
          (bb, temp)
        if s.symbol == DefsDefs.StringManipulatorFrom then
          val (nbb, valIds) = args.foldLeft((bb, List.empty[ValId]))((acc, arg) =>
            val (tbb, ret) = breakExpr(arg, acc._1)
            (tbb, acc._2 :+ ret)
          )
          val instr = newInstr(nbb)
          F.FromCall(dc.getMeth, nbb, instr, valIds(0), temp)
          (nbb, temp)
        else
          val (mbb, ret) = breakExpr(base, bb)

          val (nbb, valIds) = args.foldLeft((mbb, List.empty[ValId]))((acc, arg) =>
            val (tbb, ret) = breakExpr(arg, acc._1)
            (tbb, acc._2 :+ ret)
          )
          val instr = newInstr(nbb)
          if s.symbol.asTerm == DefsDefs.StringManipulatorPushMethod then
            F.PushCall(dc.getMeth, nbb, instr, ret, valIds(0), temp)
          if s.symbol.asTerm == DefsDefs.StringManipulatorPopMethod then
            F.PopCall(dc.getMeth, nbb, instr, ret, temp)
          if s.symbol.asTerm == DefsDefs.StringManipulatorStringMethod then
            F.StrCall(dc.getMeth, nbb, instr, ret, temp)
          (nbb, temp)
      case id: Ident =>
        val (nbb, valIds) = args.foldLeft((bb, List.empty[ValId]))((acc, arg) =>
          val (tbb, ret) = breakExpr(arg, acc._1)
          (tbb, acc._2 :+ ret)
        )
        val instr = newInstr(nbb)

        val temp = to.getOrElse(newTemp)
        valIds.zipWithIndex.foreach((valId, i) =>
          F.ActualArg(dc.getMeth, nbb, instr, i, valId)
        )
        F.Call(dc.getMeth, nbb, instr, ST.defId(id.symbol.asTerm))
        F.ActualRet(dc.getMeth, nbb, instr, temp)
        (nbb, temp)
      case _ =>
        println(fun)
        ???  // Missing cases?


  // Logic copied from tastyquery
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
