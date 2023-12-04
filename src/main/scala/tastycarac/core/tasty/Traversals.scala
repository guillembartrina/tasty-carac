package tastycarac.core.tasty

import tastyquery.Contexts.Context
import tastyquery.Symbols.{PackageSymbol, TermOrTypeSymbol}
import tastyquery.Trees.Tree
import tastyquery.Traversers.TreeTraverser


object Traversals:

  def toplevels(ps: PackageSymbol)(using Context): List[TermOrTypeSymbol] =
    ps.declarations.flatMap{
      case ps: PackageSymbol => toplevels(ps)
      case ts: TermOrTypeSymbol => List(ts)
    }

  def traverse(tree: Tree)(fun: Tree => Unit): Unit =
    (
      new TreeTraverser:
        override def traverse(tree: Tree): Unit =
          fun(tree)
          super.traverse(tree)
    ).traverse(tree)

  def traverseWithContext[C](tree: Tree, initial: C)(fun: (Tree, C) => C): Unit =
    (
      new TreeTraverser:
        var context: C = initial
        override def traverse(tree: Tree): Unit =
          context = fun(tree, context)
          super.traverse(tree)
    ).traverse(tree)
