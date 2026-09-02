## Overview

De-identification in SIM on FHIR removes or transforms identifying information before hospital data leaves the institution for secondary use. This is **pseudonymisation** under the GDPR: the exported data cannot be traced back to an individual without additional information that the hospital retains and does not share.

This page describes how de-identification is controlled through three layers, how the rules are defined, and how conformance is validated before any data is exported.

### Three layers of control

Field-level control over an export is governed by three distinct layers. Each layer has a single responsibility, and each operates on the output of the layer above it.

**Layer 1 — Profile: what *can* be present.** The FHIR profile referenced by the export defines the full set of elements that may ever appear in a resource. An element that is not part of the profile can never appear in an export. This is the outer boundary — the profile is the upper limit of what is possible.

**Layer 2 — Export query: what *goes*.** Within the bounds of the profile, the export query selects — per resource type, using `_elements` — which elements are actually included in this specific export. This is a subset of the profile, chosen per dataset. An element the profile allows but the query does not select simply does not appear in the output.

**Layer 3 — De-identification: *how* it is transformed.** For every element the query selects, a de-identification rule determines how that element is transformed on its way out: hashed, date-shifted, age-clamped, floored to the first of the month, or deliberately left intact. De-identification never removes elements — removal is the query's job, not the ruleset's.

### Core principles

The model rests on four rules that together guarantee no element leaves the hospital without an explicit, documented decision.

**Every selected element must be covered.** Each element selected by the export query must be covered by a de-identification rule — either a rule that names its path specifically, or a broad rule whose path expression matches it (for example, `**.ofType(date)` covers every element of type `date`). An element with no covering rule is a validation error: the export request cannot be released.

**`none` is allowed, but must be justified.** An element may be exported unchanged, but only through an explicit `none` action carrying an `exceptionReason` that documents why the element is safe to release intact. There is no silent pass-through — leaving an element unchanged is always a deliberate, recorded choice.

**Rules that match nothing are ignored.** A de-identification rule whose path matches no element selected by the query has no effect and produces no error. This keeps a shared standard ruleset usable across exports that select different subsets of elements.

**Coverage is made visible.** For every export, coverage can be shown element by element: each selected element, the rule that covers it, and the resulting action. This lets a privacy officer confirm, at review time, that every element is accounted for.

---

## De-identification actions

Every de-identification rule specifies an `action`. The action determines how the element is transformed, and which parameters the rule must carry. See the [DeidentificationRuleset](StructureDefinition-deidentification-ruleset.html) logical model and the [action code system](CodeSystem-deidentification-action-cs.html) for the normative definitions; the Logical Model's invariants enforce these constraints so that an invalid combination fails validation before use.

### `none`

The element is exported unchanged. Because unchanged release of a selected element is always a deliberate decision, `none` requires an `exceptionReason`.

| Parameter | Cardinality | Meaning |
|---|---|---|
| `exceptionReason` | `1..1` | Documents why this element is safe to release intact |

Use `none` for elements that carry no identifying risk and whose clinical value depends on exact preservation — coded values such as SNOMED CT, LOINC, DBC, or UCUM units, and boolean flags.

```json
{
  "path": "Observation.code",
  "action": "none",
  "exceptionReason": "Coded concept (LOINC); no identifying information, exact value required for analysis"
}
```

### `hash`

The element value is replaced by an HMAC hash. Used for identifiers and for the references that point to them. A hash rule on a resource `id` also declares, through `propagateTo`, which reference elements must be rewritten with the same hash so that referential integrity is preserved across the export.

| Parameter | Cardinality | Meaning |
|---|---|---|
| `algorithm` | `1..1` | Hash algorithm, e.g. `hmac-sha256` |
| `propagateTo` | `0..*` | Reference paths to rewrite with the same hash (for `id` elements) |

The seed for the HMAC changes per delivery, so the same patient hashes to different values across separate exports — deliveries cannot be linked to one another. See [Pseudonymisation and the per-export seed](#pseudonymisation-and-the-per-export-seed).

```json
{
  "path": "Patient.id",
  "action": "hash",
  "algorithm": "hmac-sha256",
  "propagateTo": ["*.subject", "*.patient"]
}
```

`propagateTo` lists the reference elements that carry the patient reference across resource types. `*.subject` matches the `subject` element on any resource (Observation, Condition, Encounter, …); `*.patient` matches the `patient` element where that naming is used instead (for example on some CarePlan and Coverage references). Only these named reference paths are rewritten — references to other resource types (Organization, Practitioner) are left untouched.

### `shift`

Date and dateTime values are moved by a random offset. The offset is derived deterministically per patient per delivery, so all dates for one patient move by the same amount within a run — preserving the intervals between events — while differing across patients and across deliveries.

| Parameter | Cardinality | Meaning |
|---|---|---|
| `maxDays` | `1..1` | Maximum absolute shift in days; the offset is drawn from ±`maxDays`, never zero |

The offset is shared between related patients — for example mother and child — so that temporal relationships across linked records remain consistent.

```json
{
  "path": "**.ofType(date)",
  "action": "shift",
  "maxDays": 15
}
```

### `clamp-age`

Applies to a birth date. Birth dates that imply an age outside the given range are brought to the boundary, so that outliers (very young or very old patients) cannot be singled out.

| Parameter | Cardinality | Meaning |
|---|---|---|
| `minAge` | `1..1` | Lower age bound in years |
| `maxAge` | `1..1` | Upper age bound in years |
| `exceptionReason` | `0..1` | Required when overriding the standard range for a dataset |

The standard range is 18–85. A dataset that needs a different range must state why, through `exceptionReason` — for example a stroke (CVA) cohort extending the upper bound to 91, or a maternity cohort lowering the minimum to 0.

```json
{
  "path": "Patient.birthDate",
  "action": "clamp-age",
  "minAge": 0,
  "maxAge": 91,
  "exceptionReason": "CVA dataset; stroke cohort requires extended upper age bound"
}
```

### `first-of-month`

Floors a date to the first day of its month, removing day-level precision while keeping month and year. Applies to any `date` element, not only `Patient.birthDate`; birth date is the most common target, but admission months or other coarse dates may use it too.

| Parameter | Cardinality | Meaning |
|---|---|---|
| *(none)* | | No parameters |

```json
{
  "path": "Patient.birthDate",
  "action": "first-of-month"
}
```

---

## Applying multiple rules to one element

More than one rule may target the same element. When they do, they are applied in a fixed order regardless of the order they appear in the ruleset, so the outcome is deterministic:

1. **`shift`** — move the date by the per-patient offset
2. **`first-of-month`** — floor the (shifted) date to the first of its month
3. **`clamp-age`** — bound the resulting age to the allowed range

This order is deliberate. Shifting first keeps intervals intact; flooring next removes day precision; clamping last ensures the final released value respects the age bounds no matter what the earlier steps produced. The `priority` field on a rule is available for finer control, but for date elements this canonical order is the default and covers the standard cases.

```
① shift          2024-03-22  →  2024-03-09   (offset −13 days)
② first-of-month 2024-03-09  →  2024-03-01
③ clamp-age      age from 2024-03-01 within 18–85?  →  keep / bound
```

For a plain date element (not a birth date) that has both a shift and a first-of-month rule, the same order applies: shift first, then floor. The `clamp-age` step is birth-date specific, since only a birth date implies an age.

---

## The standard ruleset and per-export overrides

*This section is non-normative. It describes one convenient way to produce a conforming effective ruleset — the approach taken by FENIX, the Santeon reference implementation.*

### One standard, shipped with the IG

The IG publishes a single **standard de-identification ruleset** as a conformance resource: the [`santeon-default`](Binary-santeon-default.html) instance of the `DeidentificationRuleset` Logical Model, versioned alongside the IG and addressable by canonical URL (`https://ig.santeon.nl/sim-on-fhir/DeidentificationRuleset/santeon-default`). It defines the default treatment for the elements common to every Santeon export — identifiers, dates, birth date.

Every export inherits this ruleset by default. An export that is happy with the defaults writes no rules of its own; it simply references the standard set.

Reading it: patient identifiers are hashed; every date is shifted by up to 15 days; birth date is additionally floored to the first of its month and then clamped to the 18–85 age range. This is the baseline every export starts from.

### Overriding per export

An export request references the standard set as its `base` and, where it needs to differ, lists `overrides`. The override is the human-authored source; it is resolved against the base to produce the effective ruleset.

An override interacts with the base by **path and action**:

| Situation | Result |
|---|---|
| Override path + action matches a base rule | Override **replaces** that base rule |
| Override path is new (no base rule for it) | Override is **added** on top of the base |
| Override sets `action: none` on a base path | Base rule for that path is **disabled** (element released intact — reason required) |

```yaml
# geboortezorg-2024.yaml — maternity dataset

de-identification:
  base: "https://ig.santeon.nl/sim-on-fhir/DeidentificationRuleset/santeon-default|0.1.0"
  overrides:
    - path: "Patient.birthDate"
      action: "clamp-age"
      minAge: 0
      maxAge: 45
      exceptionReason: "Maternity cohort; includes newborns and excludes patients over 45"
```

Here the maternity export keeps everything from the standard set — the hash rules, the date shift, the first-of-month flooring — and changes only the age clamp, from 18–85 to 0–45. Every other rule is inherited untouched.

### Resolving base + overrides into the effective ruleset

The base and the overrides are merged, by path and action, into a single **effective ruleset** — the self-contained resource that lists every rule actually in force. This effective ruleset is the artifact the IG standardises (see [Conformance](#conformance)); the base-plus-override form is input that produces it.

```
Standard ruleset (base)            Per-export overrides
santeon-default v0.1.0             geboortezorg-2024
  Patient.id → hash                  Patient.birthDate → clamp 0–45
  Patient.identifier → hash            (exceptionReason: maternity cohort)
  **.ofType(date) → shift 15
  Patient.birthDate → first-of-month
  Patient.birthDate → clamp 18–85
            │                                │
            └────────────  merge  ───────────┘
                    by path + action
                          │
                          ▼
        Effective ruleset (self-contained)
          Patient.id → hash                    (inherited)
          Patient.identifier → hash            (inherited)
          **.ofType(date) → shift 15           (inherited)
          Patient.birthDate → first-of-month   (inherited)
          Patient.birthDate → clamp 0–45       ← REPLACED by override
```

---

## Conformance

*This section is **normative**. It defines the artifacts this IG standardises and the properties a conforming export must have. It does not prescribe how a server produces the artifacts or applies the rules — only what is exchanged and what must be true of the exported data.*

### The export is described by two resources

A conforming export is fully described by two FHIR resources, exchanged together:

1. **The export `Parameters`** — a standard [FHIR Bulk Data](https://hl7.org/fhir/uv/bulkdata/) `Parameters` resource that defines *what is requested*: `_type` (which resource types), `_typeFilter` (which records within a type), and **`_elements`** (which elements of each type are released). `_elements` is core FHIR and needs no extension by this IG.

2. **The effective `DeidentificationRuleset`** — a fully-resolved instance of the [`DeidentificationRuleset`](StructureDefinition-deidentification-ruleset.html) Logical Model that defines *how each released element is transformed*.

Neither resource is sufficient alone. The `Parameters` state which elements leave the hospital but not how they are protected; the ruleset states how elements are transformed but not which are requested. Only together do they describe the export completely — and only together can coverage be checked.

```
Parameters (_type, _typeFilter, _elements)   →  WHAT is released
        +
DeidentificationRuleset (effective)          →  HOW each element is transformed
        =
        complete, checkable export
```

### The effective ruleset is self-contained

"Effective" means fully resolved: the `DeidentificationRuleset` carries the actual rules that apply, with no reference to a base ruleset plus a set of differences. Any layering a producer uses to arrive at it — a shared default, per-export overrides, inheritance — is resolved before exchange. A consumer receives one `DeidentificationRuleset` and needs nothing further to know how the data was de-identified.

A conforming effective `DeidentificationRuleset`:

- validates against the `DeidentificationRuleset` Logical Model, including all per-action invariants;
- contains no rule with `action = none` that lacks an `exceptionReason`.

### Coverage — binding the two resources

**Coverage is the requirement that binds `Parameters` to the ruleset.** For every element released by the export — every element named or implied by `_elements`, for every type in `_type` — the effective `DeidentificationRuleset` must contain at least one rule that covers it.

**Coverage is hierarchical.** A rule covers the element its `path` names *and every element beneath it*. A rule on a composite element therefore covers its whole subtree: a `none` rule on `Observation.code` covers `Observation.code.coding`, `Observation.code.coding.system`, `Observation.code.coding.code`, and any other descendant, without a separate rule for each. A released element is covered when a rule exists on that element **or on any of its ancestors**. A rule may also cover by a path expression that matches across the tree — for example `**.ofType(date)` covers every `date` element wherever it occurs.

An export whose `Parameters` release an element that is covered by no rule — neither on the element itself, nor on any ancestor, nor by a matching expression — is **non-conformant**. The data must not be released in that state.

Because coverage descends, a consumer does not need to expand `_elements` down to every leaf to check conformance: it is enough that each released path is covered at some level at or above it. A rule on a selected composite element discharges the requirement for everything that element contains.

### Which rule applies — most specific wins

Coverage says an element is *protected*; it does not by itself say *how*, because more than one rule can cover the same element — a broad ancestor rule and a narrower descendant rule at once. The transformation applied to an element is the one from **the most specific covering rule**: the rule whose `path` is deepest along that element's own path.

A rule on an ancestor sets the default for its subtree; a more specific rule deeper in the tree overrides it locally, for the element it names and that element's own descendants.

```
Rule A:  Observation.code              → none         (covers the whole code subtree)
Rule B:  Observation.code.coding.code  → hash         (covers just this leaf and below)

Applied:
  Observation.code.text                → none   (nearest ancestor rule is A)
  Observation.code.coding.system       → none   (nearest ancestor rule is A)
  Observation.code.coding.code         → hash   (rule B is more specific than A)
```

This is the same "specific over general" principle the ruleset uses to order execution — `Patient.birthDate` taking precedence over `**.ofType(date)` — now stated for coverage as well: a rule at any level covers its subtree, and the deepest rule on any given path determines the action.

> Where the deepest covering rule is a path expression rather than a named path (for example `**.ofType(date)`), it competes on the specificity of the element it matches: a rule naming `Patient.birthDate` directly is more specific than `**.ofType(date)` for that element, and wins.

Note that when the deepest covering rule is a `none`, all descendants inherit that `none` — the whole subtree is released intact, and the single `exceptionReason` on that ancestor rule accounts for the entire subtree. A `none` high in the tree therefore covers a great deal; this is intended (one reason for a whole coded concept, not one per sub-element) but worth attention at review.

### What this IG does not constrain

To keep the model implementable on any FHIR server, the following are **out of scope** of the normative content:

- how the `_elements` selection and the effective ruleset are authored or assembled (layering, defaults, overrides, templating);
- the file formats, repositories, or review processes a producer uses;
- the orchestration of an export run (how it is triggered, polled, transported);
- whether coverage is enforced ahead of time or by construction.

A server conforms by exchanging a valid `Parameters` and a valid effective `DeidentificationRuleset`, and by releasing data that satisfies coverage — regardless of how it reaches that result.

---

## Implementation notes (non-normative)

*This section describes how **FENIX**, the Santeon reference implementation, produces a conforming export. It is illustrative. Another server may reach the same result by entirely different means; nothing here is required for conformance.*

### Authoring: base plus overrides

FENIX does not author the effective ruleset directly. It starts from the shared standard ruleset published with the IG (`santeon-default`) and applies per-export overrides expressed in YAML. Overrides replace, add, or disable rules by path and action. This keeps each export request short — an export states only how it differs from the default.

The base-plus-override YAML is FENIX input, not an IG artifact. It is one convenient way to produce an effective ruleset; it is never exchanged and carries no normative status.

### Generation: resolving to the exchanged resources

`fenix generate` resolves base and overrides into the effective `DeidentificationRuleset`, and writes the `Parameters.json` carrying `_type`, `_typeFilter` and `_elements`. Together these two generated resources are the normative pair; the YAML that produced them is FENIX input and is not exchanged.

```
requests/
└── geboortezorg-2024/
    ├── geboortezorg-2024.yaml           ← FENIX input: base + overrides (non-normative)
    ├── Group.json                        ← generated
    ├── Parameters.json                   ← generated: _type, _typeFilter, _elements (IG artifact)
    └── DeidentificationRuleset.json      ← generated: effective ruleset (IG artifact)
```

A server that already holds effective rulesets — hand-authored, templated, or produced by other tooling — needs none of this. It exchanges the `Parameters` and the `DeidentificationRuleset` and exports conforming data; the path it took is its own concern.

### Coverage report

To make the coverage property auditable, FENIX emits a coverage report for each export: every released element, the rule that covers it, and the resulting action. A reviewer confirms at a glance that no element is uncovered and that every `none` carries a reason.

The report is a FENIX convenience, not an IG artifact — it is one way to demonstrate the coverage property the IG requires of the data. Another implementation might prove coverage differently, or enforce it by construction and produce no report at all.

| Element (released) | Covered by rule | Action |
|---|---|---|
| `Patient.id` | `Patient.id` | hash |
| `Patient.birthDate` | `Patient.birthDate` (×2) | first-of-month → clamp-age |
| `Observation.effectiveDateTime` | `**.ofType(date)` | shift |
| `Observation.code` | `Observation.code` | none (reason recorded) |

---

## Pseudonymisation and the per-export seed

*This section is **normative** for the properties an export must exhibit (the seed behaviour), and marks clearly what is **out of scope** (the key and seed material themselves).*

### Why hashing is keyed

Identifiers are removed by replacing them with an HMAC (see the [`hash`](#hash) action). HMAC is a *keyed* hash: the output depends on both the input value and a secret key. This is deliberate. A plain, unkeyed hash of an identifier would be reversible by anyone who can hash candidate values — the space of BSNs or patient numbers is small enough to enumerate. Keying the hash with a secret the attacker does not hold defeats that: without the key, the output cannot be linked back to the input.

The result is **pseudonymisation** under the GDPR, not anonymisation. The data cannot be re-identified from its own contents, but a party holding the key and the source data can reverse it. That key is the "additional information" the regulation requires be kept separately — and this IG requires exactly that separation.

### The per-export seed

Each export derives its HMAC seed freshly, per export run. The seed governs both the identifier hashing and the per-patient date-shift offset. Two properties follow, and a conforming export **must** exhibit both:

**Consistent within a run.** Within a single export, the same source identifier always hashes to the same value, and all of one patient's dates shift by the same offset. Referential integrity is preserved — a hashed `Patient.id` and every reference propagated to it (`*.subject`, `*.patient`) resolve to the same value — and temporal relationships between a patient's events are kept intact. Related patients (for example mother and child) share the offset, so cross-record intervals also survive.

**Unlinkable across runs.** Because the seed changes per export, the *same* patient exported in two separate runs hashes to two *different* values, and their dates shift by different offsets. A party holding two deliveries cannot tell that a record in one corresponds to a record in the other by comparing hashes or dates. Deliveries are not linkable to each other from their contents alone.

```
Patient BSN 123456789

Export run A (seed Sₐ):   hash → 8f3c1d…    birthDate shift −11d
Export run B (seed S_b):  hash → b0a927…    birthDate shift +6d

Same patient, two runs, no common value to link them on.
```

This unlinkability is a required property, not an implementation detail: an export that reused a seed across runs — producing stable hashes that let deliveries be joined — would not conform.

### Key and seed are out of scope

The HMAC key, and any seed derived from it, are **secrets held inside the hospital**. This IG does not define, carry, or exchange them, and a conforming export **must not** include them:

- the key **must not** appear in any exchanged resource — not in the `DeidentificationRuleset`, not in the `Parameters`, not in a `Provenance`, nowhere in the export payload;
- the key **must not** be committed to version control or embedded in any authored artifact;
- the key is held only in the hospital's own secret storage (an environment secret, a vault) and never crosses the institutional boundary.

Placing the key anywhere it travels with the data would defeat pseudonymisation entirely — the secret that protects the data would accompany the data it protects. Keeping it strictly inside the hospital is what makes the exported data safe to release.

The **seed** is likewise never exchanged. Only its *effect* leaves the hospital — the hashed values and shifted dates in the output. From those, the seed cannot be recovered.

### Internal reversibility — the run identifier

Pseudonymisation must be reversible by the hospital when there is a lawful basis to re-identify a patient. This IG supports that without exposing any secret, by separating the two things reversal needs: a **public label** that may travel, and the **secret key** that never does.

Each export run carries a **run identifier** — an opaque, non-secret value that names the run. The run identifier reveals nothing on its own: it is not derived from patient data and cannot be used to compute a hash or an offset. It may appear in a `Provenance` resource or in export metadata, and it may be exchanged.

Reversal is possible only inside the hospital, by combining three things it alone holds together:

```
run identifier   (names which run → which seed generation)
      +
HMAC key         (the hospital-held secret, never exported)
      +
source data      (the original identifiers, held in the EPD)
      =
re-identification of a specific patient — inside the hospital only
```

A privacy officer with access to all three can, for a named run, reconstruct the seed and reverse a specific hash or date shift back to the source patient. A party outside the hospital holds at most the run identifier and the de-identified output — never the key — and so can never reverse anything. The run identifier makes reversal *auditable and addressable* ("reverse patient X in run f3a1c8") without making it *possible* for anyone lacking the key.

> The run identifier is a non-secret handle, not key material. Exchanging it does not weaken pseudonymisation. The seed itself is derived from the key and the run inside the hospital and is never stored in or alongside an exchanged resource.

---

## Normative artifacts

The de-identification model is defined as a set of compilable FHIR Shorthand artifacts, published with this IG:

- [DeidentificationRuleset](StructureDefinition-deidentification-ruleset.html) — the Logical Model, with its per-action invariants
- [DeidentificationActionCS](CodeSystem-deidentification-action-cs.html) — the code system for `rule.action`
- [DeidentificationActionVS](ValueSet-deidentification-action-vs.html) — the value set bound to `rule.action`
- [santeon-default](StructureDefinition-deidentification-ruleset-examples.html) — the standard ruleset shipped with the IG, as a `DeidentificationRuleset` instance

Source: [`input/fsh/DeidentificationRuleset.fsh`](https://github.com/SanteonNL/sim-on-fhir/blob/main/input/fsh/DeidentificationRuleset.fsh).
