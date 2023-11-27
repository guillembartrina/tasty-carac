package tastycarac.core

import scala.annotation.tailrec

import java.nio.file.{Path, Paths, FileSystems}
import java.net.URI

import tastyquery.jdk.ClasspathLoaders
import tastyquery.Classpaths.*
import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.Symbols.*

import datalog.dsl.{Program, Constant, Atom, Relation}


class Tasty(target: List[Path], sets: Set[FactSet | RuleSet])(using Program):
  val id: Int = Tasty.nextId
  def patch(name: String): String = s"${id}#${name}" 
  val program: Program = summon[Program]

  {
    val classpath = ClasspathLoaders.read(target)
    val context = Context.initialize(classpath ++ Tasty.defaultClasspath)
    val rootPackage = context.defn.RootPackage
    
    val digraph = Digraph.merge(sets.map(Tasty.extractDigraph))

    digraph.toposort match
      case None => throw Exception("Provided sets exhibit circular dependencies")
      case Some(order) =>
        order.foreach{
          case fs: FactSet => fs.extract(rootPackage)(using context)(using this)
          case rs: RuleSet => rs.define(using this)
        }
  }

  def get(getter: Tasty ?=> Relation[Constant]): Relation[Constant] = getter(using this)

object Tasty:
  private var counter: Int = 0
  private def nextId: Int =
    counter += 1
    counter

  private val defaultPaths: List[Path] =
    FileSystems.getFileSystem(URI.create("jrt:/")).nn.getPath("modules", "java.base").nn
      :: sys.env.get("TASTYCARAC_DEFAULTCLASSPATH").get.split(";").map(Paths.get(_)).toList
  private val defaultClasspath: Classpath = ClasspathLoaders.read(defaultPaths)

  private def extractDigraph(node: FactSet | RuleSet): Digraph[FactSet | RuleSet] =
    def rec(node: FactSet | RuleSet, acc: Set[FactSet | RuleSet]): Digraph[FactSet | RuleSet] =
      if acc.contains(node) then Digraph(Set(node), Set.empty)
      else
        node match
          case fs: FactSet => Digraph(Set(fs), Set.empty)
          case rs: RuleSet =>
            val subdigraphs = rs.dependencies.map(rec(_, acc + node))
            Digraph(
              subdigraphs.map(_.nodes).flatten + node,
              subdigraphs.map(_.arcs).flatten ++ rs.dependencies.map(_ -> node)
            )
    rec(node, Set.empty)


extension (program: Program)
  def loadTasty(target: List[Path], sets: Set[FactSet | RuleSet]): Tasty = new Tasty(target, sets)(using program)
