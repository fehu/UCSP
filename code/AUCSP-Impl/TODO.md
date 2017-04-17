TODO
====

- [x] `Contexts` depend on coherence numeric type `a`, therefore `AgentState`
      depends as well. The problem is `type RoleState r = AgentState r a`.

- [ ]

### SharedSchedule: Interface

Receive:

- [x] TryPutCandidate
- [ ] TryPutCandidateWithEvidence

Send:

- [x] TryPutClasses
- [ ] TryPutClassesWithEvidence
- [x] GetClassesOfGroup

### SharedSchedule: Internal

Receive:

- [ ] ScheduleHolderListAndReset
- [x] TryPutClasses
- [ ] TryPutClassesWithEvidence
- [ ] GetClassesOfGroup

Send:

- [ ] CandidatesChanges

### SharedSchedule: Observer

Receive:

- [x] CandidatesChanges

Send:

- [ ] ScheduleObserverDemand
