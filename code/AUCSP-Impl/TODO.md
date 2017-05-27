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
- [x] CandidatesChanges

### SharedSchedule: Internal

Receive:

- [x] ScheduleHolderListAndReset
- [x] TryPutClasses
- [ ] TryPutClassesWithEvidence
- [x] GetClassesOfGroup

### SharedSchedule: Observer

Receive:

- [x] CandidatesChanges

Send:

- [ ] ScheduleObserverDemand





#### General

- [ ] check `fold` usage
