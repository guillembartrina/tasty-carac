package tastycarac.core

import scala.annotation.tailrec


case class Digraph[A](nodes: Set[A], arcs: Set[(A, A)]):
  require(arcs.map(a => Set(a._1, a._2)).flatten.subsetOf(nodes))

  private val preds: Map[A, Set[A]] =
    arcs.foldLeft
      (nodes.map(n => (n, Set.empty[A])).toMap)
      ((acc, a) => acc.updatedWith(a._2)(_.map(_ + a._1)))

  private val succs: Map[A, Set[A]] =
    arcs.foldLeft
      (nodes.map(n => (n, Set.empty[A])).toMap)
      ((acc, a) => acc.updatedWith(a._1)(_.map(_ + a._2)))

  def acyclic: Boolean =
    def rec(curr: A, acc: Set[A]): Boolean =
      acc.contains(curr) || succs(curr).exists(rec(_, acc + curr))
    nodes.exists(rec(_, Set.empty))

  def toposort: Option[List[A]] =
    @tailrec
    def rec(preds: Map[A, Set[A]], acc: List[A]): Option[List[A]] =
      val (nopreds, somepreds) = preds.partition(_._2.isEmpty)
      if nopreds.isEmpty
      then if somepreds.isEmpty then Some(acc) else None
      else
        val nexts = nopreds.keys
        rec(somepreds.mapValues(_ -- nexts).toMap, acc ++ nexts)
    rec(preds, List.empty)

object Digraph:
  def empty[A]: Digraph[A] = Digraph(Set.empty, Set.empty)

  def merge[A](digraphs: Set[Digraph[A]]): Digraph[A] =
    Digraph[A](digraphs.map(_.nodes).flatten, digraphs.map(_.arcs).flatten)
