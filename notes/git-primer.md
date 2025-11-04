---
title: A little `git` cheatsheet
subtitle: Programming Language Technology, DAT151/DIT231
---


Basics
======

## Git overview

Picture 1:

    Working dir    Git database   Remotes
    -----------    ------------   -------
    project/       project/.git   git@git.chalmers.se:user/project
    - README                      https://github.com/joe/project
    - File1
    - Dir1/File2

Picture 2: a version tree

    * - * - * - * - * main
        |           \ * - * - * develop
        * - * - * experiment

Commit identifiers: `<commit>`
- hash (absolute)   `4fab561`
- tag (symbolic, absolute) `<tag>`
- branch name (tip/leaf)   `<branch>`
- branch in a remote `<remote>/<branch>`, e.g. `origin/master`
- `HEAD`, `HEAD~1`, ... (relative to current branch)

## Starting a git project

- `git init`
- `git clone <remote>`

## Remotes

- Remotes have symbolic identifiers, e.g.:

        <remote>  <url>
        origin    git@git.chalmers.se:user/project
        joe       https://github.com/joe/project

  `origin` is default remote.

- `git remote -v`: list remotes


## Commits

Changes in the working directory are first added to the _staging area_ (the _index_)
and then bundled into a commit.  Usally, this is done with GUI support.

- `git add <file>`      : add a new file or add the changes made in the file
- `git commit -m <msg>` : create a new commit from the staged changes
- `git commit --amend`  : add staged changes to the last commit

- `git push`            : push current branch to its remote (default `origin`)
- `git pull`            : pull current branch from its remote


## Tags

- `git tag -v`                : list tags
- `git tag -a <tag> -m <msg>` : add new tag named `<tag>` with message `<msg>`
- `git push --follow-tags`    : push "missing but relevant" tags to `origin`

Somewhat advanced
=================

## Branches

Default branch is often named `master` or `main`.

- `git branch -v`             : list branches
- `git branch <branch>`       : fork of new branch, but stay on current branch
- `git checkout <branch>`     : switch to existing branch

- `git checkout <commit>`     : switch to an arbitray commit in the commit tree
- `git switch -c <branch>`    : create a new branch at the current commit and switch to it
- `git checkout -b <branch>`  : same

- `git push <remote> <branch>`    : push branch to remote
- `git push -u <remote> <branch>` : push branch to remote which is set as default remote

- `git pull <remote> <branch>`    : pull remote branch into current branch


## Pulling with conflicts

- `git pull --ff-only`            : only pull if remote does not have new commits
- `git pull --merge`              : pull and create a merge commit reconciling remote and local commits
- `git pull --rebase`             : pull, but rebase current branch while adding commits from remote

## Merging

- `git merge <branch>`     : merge `<branch>` into the current branch
  E.g. if you are on a feature branch, `git merge master` will merge the changes from `master`
  into your feature branch.
- `git rebase <branch>`    : merge `<branch>` into the current branch, keeping your commits on top
- `git rebase -i <commit>` : interactive rebase, allows to modify your commits

## Undoing

- `git revert <commit-range>` : create a commit that undoes the changes of the given commits
- `git reset --soft HEAD~1`   : undo the last commit, keep changes in the working dir
- `git reset --hard <commit>` : reset the current branch and working dir to a different commit
- `git reflog`                : show latest history

There is also `git stash`, but you need to know what you are doing...

## Managing remotes

- `git remote -v`                         : list remotes
- `git remote add <remote> <url>`         : add new remote to the list
- `git remote rename <remote1> <remote2>` : rename remote `<rid1>` to `<rid2>`
- `git remote set-url <remote> <url>`     : bind remote to new url
