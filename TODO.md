# TODO

- [x] actually there are 3 lists of goals
  - active goals
  - delayed goals
    - define predicate for what counts as a goal to delay
  - failed goals (didn't unify with any rule conclusions)
  - [ ] write test that demonstrates how goals are delayed
- [ ] data structures for "unification factoring"
  - storing an indexing data structure for efficiently unifying with a set of
    terms
- [x] demo delay and resume
  - viz module `Spec.Engine.DelayAndResume`
- [ ] demo finding multiple solutions
  - parsing should work for this
- [ ] demo constructing a proof term as one of the components of atom
  - parsing _could_ also work for this lol
