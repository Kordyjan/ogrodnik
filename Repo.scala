package ogrodnik

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

import os.Path
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.transport.sshd.SshdSessionFactory
import org.eclipse.jgit.transport.SshSessionFactory
import org.eclipse.jgit.transport.sshd.JGitKeyCache
import org.eclipse.jgit.transport.sshd.DefaultProxyDataFactory
import org.eclipse.jgit.api.RebaseCommand.InteractiveHandler
import org.eclipse.jgit.lib.RebaseTodoLine
import java.{util => ju}
import org.eclipse.jgit.api.CreateBranchCommand.SetupUpstreamMode
import org.eclipse.jgit.api.RebaseCommand
import org.eclipse.jgit.api.RebaseResult

class Repo private (private[ogrodnik] val git: Git):
  def resolve(rev: String): Commit =
    val repository = git.getRepository()
    val id = repository.resolve(rev)
    Commit(repository.parseCommit(id))(using this)

  def sync(ref: String): Unit =
    if (git.branchList().call().asScala.exists(_.getName() == s"refs/heads/$ref")) then
      git.checkout().setName(ref).call()
    else
      git.checkout().setName(ref)
        .setCreateBranch(true)
        .setUpstreamMode(SetupUpstreamMode.SET_UPSTREAM)
        .setStartPoint(s"origin/$ref").call()
    val res = git.pull().setRebase(true).call()
    if !res.isSuccessful() then
      throw new Exception(s"Failed to pull:\n $res")

  def syncWithBase(ref: String, base: String): Unit =
    if (git.branchList().call().asScala.exists(_.getName() == s"refs/heads/$ref")) then
      git.checkout().setName(ref).call()
    else
      git.checkout().setName(ref)
        .setCreateBranch(true)
        .setUpstreamMode(SetupUpstreamMode.SET_UPSTREAM)
        .setStartPoint(s"origin/$ref").call()
    val res = git.rebase().setUpstream(base).call()
    if !(Set("UP_TO_DATE", "FAST_FORWARD", "OK").contains(res.getStatus().toString())) then
      throw new Exception(s"Failed to sync:\n ${res.getStatus()}")

  def linearHistory(include: String, exclude: String)(
      mergeFilter: Commit => Boolean
  ) =
    val repository = git.getRepository()
    val shas = git
      .log()
      .add(repository.resolve(include))
      .not(repository.resolve(exclude))
      .call()
      .asScala
      .map(_.name())
      .toSet

    val visited = mutable.Set.empty[String]

    def visit(c: Commit): List[Commit] =
      if visited.contains(c.sha) || !shas.contains(c.sha) then Nil
      else
        visited.add(c.sha)
        c :: visit(c.parents(0))

    def fillSidebranches(commits: List[Commit]): List[Commit] =
      val visitedBefore = visited.size
      val res = commits.flatMap: c =>
        if c.parents.size == 1 then List(c)
        else if mergeFilter(c) then visit(c.parents(1))
        else Nil
      if visited.size == visitedBefore then res else fillSidebranches(res)

    val start = resolve(include)
    fillSidebranches(visit(start))
  end linearHistory

  def onBranch[T](branch: String)(block: (Repo, OnBranch) ?=> T): T =
    git.checkout().setName(branch).call()
    block(using this, OnBranch(branch))

  def onNewBranch[T](branchPoint: String, name: String)(block: (Repo, OnBranch) ?=> T): T =
    git.checkout().setName(branchPoint).call()
    git.pull().call()
    git.checkout().setCreateBranch(true).setName(name).call()
    block(using this, OnBranch(name))


  // TODO: this is just a stub, it only work sometimes
  def rebase(branch: String, newBase: String, branchingPoint: String): Unit =
    val branchingSha = resolve(branchingPoint).sha
    println(s"branch: $branch, newBase: $newBase, branchingPoint: $branchingPoint")
    println(s"branch: ${resolve(branch).sha}, newBase: ${resolve(newBase).sha}, branchingPoint: ${resolve(branchingPoint).sha}")
    git.checkout().setName(branch).call()
    val handler = new InteractiveHandler {
      override def prepareSteps(steps: ju.List[RebaseTodoLine]): Unit =
        println("before:")
        steps.asScala.foreach: s =>
          println(s"\t${s.getAction()} ${s.getCommit().name()}")
        val toRemove = steps.asScala.indexWhere: c =>
          branchingSha.startsWith(c.getCommit().name())
        println("after:")
        steps.asScala.foreach: s =>
          println(s"\t${s.getAction()} ${s.getCommit().name()}")
        steps.asScala.remove(0, toRemove + 1)
      override def modifyCommitMessage(message: String) = message
    }
    val res = git.rebase().runInteractively(handler).setUpstream(newBase).call()
    println(res.getStatus())

object Repo:
  def open(path: Path): Repo =
    val factory = new SshdSessionFactory(new JGitKeyCache, new DefaultProxyDataFactory)
    SshSessionFactory.setInstance(factory)
    new Repo(Git.open(path.toIO))

def git(using repo: Repo): Git = repo.git
def repository(using repo: Repo): org.eclipse.jgit.lib.Repository =
  repo.git.getRepository()

sealed class OnBranch(val name: String)
