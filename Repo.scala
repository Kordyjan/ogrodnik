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

class Repo private (private[ogrodnik] val git: Git):
  def resolve(rev: String): Commit =
    val repository = git.getRepository()
    val id = repository.resolve(rev)
    Commit(repository.parseCommit(id))(using this)

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
    block(using this, new OnBranch { val name = branch })

  def onNewBranch[T](branchPoint: String, name: String)(block: (Repo, OnBranch) ?=> T): T =
    git.checkout().setName(branchPoint).call()
    git.pull().call()
    git.checkout().setCreateBranch(true).setName(name).call()
    block(using this, new OnBranch { val name = name })

object Repo:
  def open(path: Path): Repo =
    val factory = new SshdSessionFactory(new JGitKeyCache, new DefaultProxyDataFactory)
    SshSessionFactory.setInstance(factory)
    new Repo(Git.open(path.toIO))

def git(using repo: Repo): Git = repo.git
def repository(using repo: Repo): org.eclipse.jgit.lib.Repository =
  repo.git.getRepository()

sealed trait OnBranch:
  val name: String
