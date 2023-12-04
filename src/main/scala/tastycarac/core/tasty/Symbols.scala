package tastycarac.core.tasty

import scala.collection.mutable

import tastyquery.Names.Name
import tastyquery.Symbols.{Symbol, ClassSymbol, TermSymbol}


object Symbols:

  type ClassId = String
  type ValId = String
  type DefId = String
  // TypeId missing?

  private class XTable[X, XR, XId](rawize: X => XR, buildId: (XR, Int) => XId):
    private val counter: mutable.Map[XR, Int] = mutable.Map.empty
    private val x2xid: mutable.Map[X, XId] = mutable.Map.empty
    //private val id2x: mutable.Map[XId, X] = mutable.Map.empty

    def getXId(x: X): XId =
      if !x2xid.contains(x) then
        val xr = rawize(x)
        val cc = counter.updateWith(xr)(v => Some(v.fold(0)(_ + 1))).get
        x2xid.put(x, buildId(xr, cc))
      x2xid(x)

  class SymbolTable:

    // Why this and not displyFullPath from Symbol. The latter doesn't like non-static stuff, but perhaps for a reason?
    private def pseudostaticPath(symbol: Symbol): List[Name] = symbol.owner match
      case owner: Symbol => pseudostaticPath(owner) :+ symbol.name
      case null => Nil  // Skip <root>

    private def simpleId[T >: String](path: List[Name], i: Int): T =
      val name = path.mkString(".")
      if i == 0 then name else name + s"#$i"

    private val classes = new XTable[ClassSymbol, List[Name], ClassId](pseudostaticPath, simpleId)
    private val vals = new XTable[TermSymbol, List[Name], ValId](pseudostaticPath, simpleId)
    private val defs = new XTable[TermSymbol, List[Name], DefId](pseudostaticPath, simpleId)

    def classId(classSymbol: ClassSymbol): ClassId = classes.getXId(classSymbol)
    def valId(termSymbol: TermSymbol): ValId =
      require(!termSymbol.isMethod)
      vals.getXId(termSymbol)
    def defId(termSymbol: TermSymbol): ValId =
      require(termSymbol.isMethod)
      vals.getXId(termSymbol)

  val global: DefId = "?"

  def thisValId(defId: DefId): ValId = s"$defId/this"
  
  //Special name for method parameters?
