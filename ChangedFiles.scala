package ogrodnik

import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.api.Git
import scala.util.matching.Regex

class ChangedFiles(private val map: Map[String, ChangedFiles]):
  def put(path: List[String]): ChangedFiles =
    path match
      case head :: tail if !head.isBlank =>
        val dir = map.getOrElse(head, ChangedFiles.empty)
        ChangedFiles(map.updated(head, dir.put(tail)))
      case _ => this

  def print = printInternal(None)

  def print(depth: Int) = printInternal(Some(depth))

  private def printInternal(depth: Option[Int], indent: Int = 0): Unit =
    depth match
      case Some(n) if n <= 0 => // do nothing
      case _ =>
        for (k, v) <- map.toList.sortBy(_._1) do
          println("  " * indent + k)
          v.printInternal(depth.map(_ - 1), indent + 1)

  def isEmpty: Boolean = map.isEmpty

  def ++(that: ChangedFiles): ChangedFiles =
    val keys = map.keySet ++ that.map.keySet
    val newMap = keys
      .map: k =>
        k -> (map.getOrElse(k, ChangedFiles.empty) ++ that.map.getOrElse(
          k,
          ChangedFiles.empty
        ))
      .toMap
    ChangedFiles(newMap)

  override def equals(x: Any): Boolean =
    x match
      case that: ChangedFiles =>
        map == that.map
      case _ => false

  override def hashCode(): Int = map.hashCode() * 3

object ChangedFiles:
  def empty: ChangedFiles = ChangedFiles(Map.empty)

  sealed trait Filter:
    def apply(entry: ChangedFiles): ChangedFiles

  case object All extends Filter:
    def apply(entry: ChangedFiles): ChangedFiles = entry

  case class NameMatch(pattern: Regex, nested: Filter = All) extends Filter:
    def apply(entry: ChangedFiles): ChangedFiles =
      val filtered = entry.map
        .collect:
          case (k, v) if pattern.matches(k) => k -> nested(v)
      ChangedFiles(filtered)

  case class Altenative(filters: Filter*) extends Filter:
    def apply(entry: ChangedFiles): ChangedFiles =
      filters.foldLeft(ChangedFiles.empty): (acc, filter) =>
        acc ++ filter(entry)

  case class Recursive(filter: Filter) extends Filter:
    def apply(entry: ChangedFiles): ChangedFiles =
      val self = filter(entry)
      val children =
        val newMap = entry.map
          .collect:
            case (k, v) if !v.isEmpty => k -> apply(v)
        ChangedFiles(newMap.toMap)
      self ++ children
