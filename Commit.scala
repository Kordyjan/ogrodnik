package ogrodnik

import scala.jdk.CollectionConverters.*

import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.treewalk.CanonicalTreeParser
import org.eclipse.jgit.api.CherryPickResult.CherryPickStatus
import org.eclipse.jgit.merge.ResolveMerger.MergeFailureReason
import org.eclipse.jgit.api.ResetCommand.ResetType

class Commit private[ogrodnik] (commit: RevCommit)(using repo: Repo):
  private lazy val parsed =
    if commit.getRawBuffer() == null
    then repository.parseCommit(commit)
    else commit

  def message: String = parsed.getFullMessage()

  def parents: List[Commit] =
    parsed.getParents().toList.map(Commit(_))

  def sha = parsed.getId().name()

  def shortSha(length: Int = 7) = parsed.getId().abbreviate(length).name()

  def displayName =
    val lines = LazyList.from(message.linesIterator)
    val title = lines.head
    val cont = if lines.drop(1).isEmpty then "" else "..."
    s"${shortSha()} $title$cont"

  override def toString(): String = shortSha()

  override def equals(x: Any): Boolean = x match
    case c: Commit => c.sha == sha
    case _         => false

  override def hashCode(): Int = sha.hashCode()

  def changedFiles =
    git
      .diff()
      .setNewTree(treeParser)
      .setOldTree(parents.head.treeParser)
      .setShowNameOnly(true)
      .call()
      .asScala
      .foldLeft(ChangedFiles.empty): (diff, entry) =>
        diff.put(entry.getNewPath().split("/").toList)

  def cherryPick(using OnBranch) =
    val result = git.cherryPick().include(parsed).call()
    val previousHead = summon[Repo].resolve("HEAD")
    result.getStatus() match
      case CherryPickStatus.OK =>
        val newMessage = nameCherryPickedCommit(this, false)
        git.commit().setAmend(true).setMessage(newMessage).call()
        CherryPickDone
      case CherryPickStatus.CONFLICTING =>
        val conflicts = git.status().call().getConflicting().asScala.toSet
        CherryPickConflict(this, previousHead, conflicts)
      case CherryPickStatus.FAILED =>
        CherryPickError(this, translateFailReasons(result.getFailingPaths().asScala.toMap))

  private def treeParser =
    val tree = parsed.getTree()
    val parser = new CanonicalTreeParser()
    val reader = repository.newObjectReader()
    parser.reset(reader, tree)
    parser

end Commit

sealed trait CherryPickResult

object CherryPickDone extends CherryPickResult

case class CherryPickConflict(commit: Commit, previousHead: Commit, conflicts: Set[String]) extends CherryPickResult:
  def markResolved(using Repo, OnBranch) =
    git.add().addFilepattern(conflicts.mkString(" ")).call()
    val newMessage = nameCherryPickedCommit(commit, true)
    git.commit().setMessage(newMessage).call()

  def abort(using Repo, OnBranch) =
    git.reset().setMode(ResetType.HARD).setRef(previousHead.sha).call()

case class CherryPickError(commit: Commit, reason: String) extends CherryPickResult:
  def retry(using OnBranch) = commit.cherryPick

private def translateFailReasons(map: Map[String, MergeFailureReason]): String = map
  .map: (path, reason) =>
    s"${reason.toString()}: $path"
  .mkString("\n")

private def nameCherryPickedCommit(original: Commit, modified: Boolean)(using Repo) =
  val padded = original.message + (if original.message.linesIterator.length > 1 then "\n" else "\n\n")
  val newMsg = padded + s"[Cherry-picked ${original.sha}]"
  newMsg + (if modified then "[modified]" else "")
