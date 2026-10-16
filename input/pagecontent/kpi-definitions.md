## Overview

The model of this IG is not written per measurement. It is **derived from KPI definitions**. A KPI definition states which data a KPI needs; the union of all KPI definitions determines which codes, elements and records the model allows, and which records an export selects.

```text
 definition source (outside the IG model)            this IG
 ──────────────────────────────────────────          ─────────────────────────────────────────────
 ObservationDefinition  ─┐                           KpiDefinition      (logical model + instances)
 ValueSet               ─┴─ referenced by ─────────▶  DatasetDefinition  (logical model + instances)
                                                              │
                                              _generateFromDefinitions.py
                                                              │
                    ┌─────────────────────────────────────────┴────────────────────┐
         one profile per resource type                                 per dataset: Group + Parameters
         (codes, value shapes, closed elements)                        (member-filter, _type, _typeFilter, _elements)
```

This is the same split as in [De-identification](deidentification.html): the profile is the upper bound of what *can* be present (layer 1), the generated export request is what *goes* (layer 2).

## Definition source

ObservationDefinitions and ValueSets describe measurements and terminology. They are managed as FSH files next to each other, separate from the IG model, and referenced from KPI definitions by id. In this IG they live in `input/fsh/definitions/` as a stand-in for a separate definition package.

- An **ObservationDefinition** is the single source for a measurement's code, value type, unit and permitted answers. A panel (blood pressure) lists its components through the [ODComponent](StructureDefinition-od-component.html) extension, because R4 has no component element on ObservationDefinition.
- A **ValueSet** carries the terminology for cohorts and for coded answers.

R4 ObservationDefinition has no canonical URL or version. It is identified by its `id` within the definition source, and versioned together with it.

## KPI definitions

A [KpiDefinition](StructureDefinition-kpi-definition.html) is a list of criteria, each a resource type plus FHIR search parameters written with the normal search parameter names. It is **role-neutral**: nothing says whether a criterion is numerator, denominator or exclusion. That meaning belongs to the downstream calculation.

Parameter values may reference the definition source:

| Reference | Resolves to |
|---|---|
| `{vs:<id>}` | canonical URL of the ValueSet |
| `{od:<id>.code}` | `system\|code` of the ObservationDefinition |
| `{od:<id>.unit}` | `system\|code` of its quantitative unit |
| `{od:<id>.valid}` | canonical URL of its valid coded value set |

`{placeholders}` such as `{period.start}` are filled in by the exporter at run time.

**Dates relative to the export day** use `{today}` with an optional offset in days, months or years: `{today}`, `{today-18y}`, `{today-90d}`, `{today+3m}`. A cohort that needs "18 years or older at delivery" is written `birthdate=le{today-18y}`. The exporter replaces the placeholder by a concrete date when it sends the request, so the query on the server is an ordinary FHIR search (`birthdate=le2008-10-06`). `py -3 _generateFromDefinitions.py --today 2026-10-06` prints every request of every dataset with the dates filled in, to check them.

**Codes roll up.** Criteria on the same resource that differ only in their code become one `_typeFilter` with a comma-separated list of codes, for example `Observation?code=http://loinc.org|38445-3,http://snomed.info/sct|228273003&date=ge2018-01-01`.

### Terminology maintained elsewhere

Not every ValueSet is authored as FSH. Some are maintained in ART-DECOR. A criterion references such a set by its canonical URL and the generator treats it like a local one:

- `{vs:http://decor.nictiz.nl/fhir/ValueSet/<oid>--<effective>}` resolves to that canonical; nothing is copied into this repository.
- `code:in=http://decor.nictiz.nl/fhir/ValueSet/...` (without braces) is read the same way.
- In a generated code value set it becomes `include codes from valueset <canonical>`, so the generated profile follows the external set; membership is evaluated by the terminology server.
- A dataset page shows the canonical as a link, not the member codes, because the codes are not known at generation time.

A pinned `<oid>--<effective>` version in the URL fixes the content; an unpinned URL follows the latest.

### Codes inline in a cohort or criterion

A cohort that only needs two or three codes does not need a ValueSet. List them as literal tokens, for example `type=https://declaratie.nza.nl/zorgtype|11,https://declaratie.nza.nl/zorgtype|21`. They are passed through unchanged to the Group `member-filter` and shown on the dataset page. Literal codes on a `code` parameter of a *KPI* criterion are also added to the generated code value set, so the profile accepts them.

**Post-query filters.** Some logic cannot be a FHIR search: "latest per patient", or a relation between resources such as "calprotectin within 90 days before the scopie". A KPI states these in `postFilter`, in words with an optional formal expression. They are not part of the export query; they are applied after retrieval, and shown on the dataset pages so a reviewer sees the whole definition.

### Answers to a measurement

The permitted answers of a coded measurement come from one of two places, and from both when both are given:

| Where | How | Typical use |
|---|---|---|
| The ObservationDefinition | `validCodedValueSet`: a local ValueSet, or the canonical URL of one in ART-DECOR or on the NTS (registered in `external-valuesets.yaml`, so the version is pinned) | answers that belong to the measurement itself and are shared |
| The KPI definition | a `value-concept` parameter on the criterion: `value-concept=system\|code,system\|code` inline, or `value-concept:in={vs:..}` | answers that only one KPI cares about |

The generator folds all answers for one measurement into a single generated ValueSet (`santeon-answers-<od-id>`) and the profile's invariant for that measurement requires membership of it. The same `value-concept` list is also part of the export filter, so the exported records are the ones with those answers.

### One generated value set per resource type

Loose codes do not each need a ValueSet. All codes a KPI defines inline, all ObservationDefinition codes and all ValueSets the criteria refer to (local, ART-DECOR or NTS) are folded into one generated value set per resource type, such as `SanteonConditionCodeVS`. It is a result, not a source: it is never maintained by hand and does not belong in ART-DECOR. Changing a KPI changes it on the next generation.

## Datasets

A [DatasetDefinition](StructureDefinition-dataset-definition.html) describes one export: the **cohort** (criteria that become the Group's `member-filter` extensions, plus cohort logic no search can express) and the **KPIs** whose data the export releases. See [Datasets](datasets.html) for the datasets and for the derived model.

## What is generated

`_generateFromDefinitions.py` compiles the definitions with SUSHI and writes:

| Output | Content |
|---|---|
| `Model.fsh` | per resource type: a code ValueSet (union of all codes the KPIs ask for), a profile that binds `code` to it, closes every element no KPI asks for, and carries one invariant per ObservationDefinition |
| `Exports.fsh` | per dataset: a `Group` with the cohort as `member-filter`s, and a `Parameters` with `_type`, `_typeFilter`, `_elements` |
| `datasets.md`, `dataset-<id>.md` | the readable pages |

**One profile per resource type, not per measurement.** `Observation.value[x]` cannot be constrained conditionally on the code in an ordinary profile. The generated profile therefore derives one invariant from each ObservationDefinition: *if the code is X, then the value is a Quantity in unit U* (or *a CodeableConcept from value set V*, or *no value, with components*). Adding a measurement means adding an ObservationDefinition; the profile follows.

Invariants that use `memberOf` need a terminology server when the data is validated.

### Data outside a KPI

Not everything a dataset releases belongs to a KPI. The Patient model, for example, is released with a filter on gender or an age limit, without any KPI asking for it. A dataset lists such data under `include`, in the same shape as a KPI criterion: a resource, FHIR search parameters and the elements to release.

```text
* include[+].resource = #Patient
* include[=].param[+].name = "gender"
* include[=].param[=].value = "male,female"
* include[=].element[+] = "gender"
* include[=].element[+] = "birthDate"
```

Included data joins the export request (`_type`, `_typeFilter`, `_elements`) and the derived model exactly like a KPI criterion, so the generated profile for that resource accepts the elements listed and closes the rest. The age limit that decides who is in the Group stays in the `cohort`; `include` is about what is released.

## Exporting a dataset

A dataset is exported with a sequence of ordinary FHIR requests, not one call: create the cohort Group, wait until it exists, start `$export` on it with the generated Parameters, wait, download. A Bundle cannot carry this, because `$export` is asynchronous. Each dataset page lists the sequence for its own Group and Parameters. A server such as FENIX can run the sequence behind a single call of its own; that is an implementation choice and not part of the IG.

## Relation to the other layers

- **Profiles** define what may appear; the generated profiles are bounded by the KPIs.
- **De-identification** must cover every element in `_elements` and every `_typeFilter` generated here. Each dataset page lists them.
- The generator and the YAML-free flow are not part of conformance. A server that holds the Group and Parameters resources needs none of it.
