# TODO

## Current

- [ ] demo finding multiple solutions
  - parsing should work for this
- [ ] demo constructing a proof term as one of the components of atom

  - parsing _could_ also work for this lol

- [ ] data structures for "unification factoring"
  - storing an indexing data structure for efficiently unifying with a set of
    terms

## Done

- [x] actually there are 3 lists of goals
  - active goals
  - suspended goals
    - define predicate for what counts as a goal to suspend
  - failed goals (didn't unify with any rule conclusions)
  - [ ] write test that demonstrates how goals are suspended
- [x] demo suspend and resume
  - viz module `Spec.Engine.SuspendAndResume`
