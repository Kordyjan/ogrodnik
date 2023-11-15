package ogrodnik

import org.eclipse.jgit.transport.SshSessionFactory
import org.eclipse.jgit.transport.sshd.SshdSessionFactory
import org.eclipse.jgit.transport.sshd.JGitKeyCache
import org.eclipse.jgit.transport.sshd.DefaultProxyDataFactory

def push(using Repo, OnBranch) =
  val factory = new SshdSessionFactory(new JGitKeyCache, new DefaultProxyDataFactory)
  SshSessionFactory.setInstance(factory)
  git.push()
    .add(summon[OnBranch].name)
    .call()