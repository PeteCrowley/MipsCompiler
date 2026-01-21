# Contributing Guidelines

## Version Control Discipline

Our project uses the `main` branch as our primary development branch and the
`submissions` branch as snapshots of versions that are submitted to the class
instructors for grading.

New features will be implemented on separate branches before being merged into
`main`. Before beginning to implement a feature, notify the other members of the
group what you will be doing and what you wish to accomplish with the feature.
This step will eliminate code duplication.

Before being merged into `main`, a feature branch must satisfy all of these
constraints:

1. The code must compile.
2. The merge must be able to be performed free of merge conflicts (see below for
   instructions on how to resolve those).
3. The branch must be reviewed and approved by every other group member.

To merge a feature branch into `main`, perform the following steps:

### Merging and Conflict Resolution

- First, merge the `main` branch into the feature branch, resolving any
  conflicts. This will create a new merge commit on the **feature branch**.
- If any conflicts needed to be resolved, the rest of the group should re-review
  the branch (conflicts can be messy!).
- The `feature` branch should then be able to be merged via `main` via a
  fast-forward merge (`git merge --ff-only feature`).

## Tooling and Formatting

Use [`smlfmt`](https://github.com/shwestrick/smlfmt) to format your SML code.
Use [`millet`](https://github.com/azdavis/millet) to lint/validate your SML
code.
