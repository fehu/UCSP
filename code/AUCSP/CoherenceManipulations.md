# Coherence Manipulations

### Contexts

##### Obligations Relations
Boolean, combined with `and`.

[Obligations.hs](code/AUCSP/src/AUCSP/Context/Obligations.hs)

##### Preferences Relations
Fractional, within [0, 1]. Combined by multiplication.

[Preferences.hs](code/AUCSP/src/AUCSP/Context/Preferences.hs)

##### External Relation: Opinion
Coherence evaluation by other agents with details.

Contains evaluations of proposed candidate:

  * Internal coherence.
  * Coherence with current _best candidate_.

Coherence of __an opinion__ is combined from three values:

  1. Internal coherence (within [0, 1]).
  2. Coherence with best, scaled from [0, 1] to [1, 2].
  3. 10, if is coherent with best; otherwise 0.

The sum of these values lies within [0, 13] interval.
The resulting _opinion coherence_ is unit-scaled sum.

A candidate is considered coherent if its coherence value is not less than __10/13__.

[External.hs](code/AUCSP/src/AUCSP/Context/External.hs)

### Inter-Context

Coherence values, evaluated at contexts are _summed_.

[Coherence.hs](code/AUCSP/src/CSP/Coherence.hs)
