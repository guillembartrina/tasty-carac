package tastycarac.analyses

import tastyquery.Contexts.Context
import tastyquery.Symbols.PackageSymbol

import tastycarac.core.*
import tastycarac.core.tasty.*

import tastyquery.Names.*
import tastyquery.Trees.*
import tastyquery.Symbols.*

object TestFS extends FactSet:

  val facts: Set[String] = Set("TestF")

  def extract(toplevels: Set[TermOrTypeSymbol])(using Context)(using Tasty): Unit =
    F.TestF(12)
    F.TestInnerF("a", 12)


object Explorer extends FactSet:

  val facts: Set[String] = Set("Expl")

  def extract(toplevels: Set[TermOrTypeSymbol])(using ctx: Context)(using Tasty): Unit =
    def fullPath(s: Symbol): List[Name] = s.owner match {
      case owner: Symbol => fullPath(owner) :+ s.name
      case null => s.name :: Nil
    }

    val root = ctx.defn.RootPackage

    tastycarac.core.tasty.Traversals.toplevels(root)
    toplevels.flatMap(_.tree.toList).foreach(tl =>
      tastycarac.core.tasty.Traversals.traverse(tl){
        case ValDef(_, tpt, rhs, sym) =>
          println("-----")
          println(tpt.toType)
          rhs.foreach(println)
          println(sym.declaredType)
        //case ClassDef(_, template, symbol) =>
        //  println("C " + symbol.displayFullName + " " + symbol.isModuleClass)
        //case DefDef(_, _, _, rhs, sym) => rhs.foreach(x => println("D " + x));
          //println(sym.displayFullName);
          //try
          //  println(rhs.get.asInstanceOf[Block].stats(0).asInstanceOf[Assign].lhs.asInstanceOf[Ident].symbol)
          //catch _ => ()
        case _ => ()
      }
    )

    //val main = root.getPackageDecl(SimpleName("main")).get
    //val Main = main.getDecl(moduleClassName("Main")).get.asClass
    
    //val fun = Main.findNonOverloadedDecl(termName("temp"))
    //val init = Main.findNonOverloadedDecl(termName("<init>"))

    /*
    println(fun.tree.get.asInstanceOf[DefDef].paramLists(0).left.get(0).symbol.displayFullName)

    val tempapply = Main.tree.get.asInstanceOf[ClassDef].rhs.body(3)
    val tempapplyparam = tempapply.asInstanceOf[Apply].args(0).asInstanceOf[Block]
    
    println(tempapplyparam.stats(0).asInstanceOf[DefDef].symbol.displayFullName)
    println(tempapplyparam.stats(0).asInstanceOf[DefDef].symbol.owner)
    println(fullPath(tempapplyparam.stats(0).asInstanceOf[DefDef].symbol))

    println(tempapplyparam.stats(0).asInstanceOf[DefDef].paramLists(0).left.get(0).symbol.displayFullName)
    println(fullPath(tempapplyparam.stats(0).asInstanceOf[DefDef].paramLists(0).left.get(0).symbol))

    val block = Main.tree.get.asInstanceOf[ClassDef].rhs.body(4)
    val blockfirst = block.asInstanceOf[Block].stats(0).asInstanceOf[Block].stats(0).asInstanceOf[DefDef]

    println(blockfirst.symbol.displayFullName)
    println(blockfirst.paramLists(0).left.get(0).symbol.displayFullName)

    println("---")

    val v = Main.getMember(termName("v")).get

    println(v.displayFullName)

    val block2 = Main.tree.get.asInstanceOf[ClassDef].rhs.body(6)
    val block2first = block2.asInstanceOf[Block].stats(0).asInstanceOf[ValDef]

    println(block2first.symbol.displayFullName)

    val block3 = Main.tree.get.asInstanceOf[ClassDef].rhs.body(8)
    val block3first = block3.asInstanceOf[Block].stats(0).asInstanceOf[ClassDef]

    println(block3first.rhs.body(0).asInstanceOf[ValDef].symbol.displayFullName)
    println(fullPath(block3first.rhs.body(0).asInstanceOf[ValDef].symbol))
    */

    /*
    val meth = Main.getNonOverloadedDecl(termName("meth")).get.tree.get.asInstanceOf[DefDef]

    println(meth.paramLists(0).left.get(0).symbol.displayFullName)
    println(fullPath(meth.paramLists(0).left.get(0).symbol))
    */

    //val block3 = Main.tree.get.asInstanceOf[ClassDef].rhs.body(10)
    //val block3first = block3.asInstanceOf[Block].expr.asInstanceOf[Block].stats(0).asInstanceOf[DefDef]

    //println(block3first.symbol.signedName)
    //println(fullPath(block3first.symbol))

    F.Expl(0)


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