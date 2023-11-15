package ogrodnik

def push(using Repo, OnBranch) =
  git.push()
    .add(summon[OnBranch].name)
    .call()