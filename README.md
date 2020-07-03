# Haskell Stack .stack-work cache management helper

## Motivation

Switching branches often triggers significant rebuilds. While this is not an issue for small projects, it gets quite frustrating for larger codebases with long build times (such as half an hour).

## Solution

Have separate `.stack-work` per branch. We'd also like to reuse existing cache for new branches – we copy cache from `master` branch using Copy-on-Write strategy available on APFS (macOS) and some Linux filesystems (e.g. btrfs).

Note: We could not use `--work-dir` Stack flag, since changing this also triggers rebuilds – this prevents from copying cache between branches.

As a bonus, we also exclude stack cache from Time Machine backups.

## Setup

1. Add stack-cache to $PATH
2. Perform stack-cache init from the directory with stack.yaml
3. Add post-checkout git hook to call `stack-cache relink` on branch change (with appropriate working directory)

4. Make sure to ignore stack-cache directories:
```shell
git config --global core.excludesfile ~/.gitignore
echo .stack-work-cache >> ~/.gitignore
echo .stack-work >> ~/.gitignore
```

5. Periodically run stack-cache clean-all to free some disk space
