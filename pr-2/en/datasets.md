# Datasets - SIM on FHIR v0.1.0

## Datasets

## Datasets

One dataset per disease area. Each dataset is exported on its own Group; its definition, cohort, KPIs, post-query filters and resolved export request are on its page. How the definitions work is described under [KPI definitions](kpi-definitions.md).

| | | | |
| :--- | :--- | :--- | :--- |
| [IBD](ibd-dataset.md) | 0.1.0 | 3 | 2 |

### KPI definitions in use

| | | |
| :--- | :--- | :--- |
| [Alcoholgebruik vastgelegd](Binary-alcohol-status-recorded.md) | 0.1.0 | [IBD](ibd-dataset.md) |
| [K3.1.2 Scopie](Binary-k312-scopie.md) | 0.1.0 | [IBD](ibd-dataset.md) |
| [K3.6.3 Calprotectine voor scopie](Binary-k363-calprotectin-before-scopie.md) | 0.1.0 | [IBD](ibd-dataset.md) |

-------

### Derived model

The union of all KPI definitions determines the model. One profile per resource type is generated; it accepts exactly the codes the KPIs ask for and closes every element no KPI asks for.

#### Observation

Profile: [Observation (SAN KPI)](StructureDefinition-observation-san-kpi.md) · code value set: [Observation code (SAN KPI)](ValueSet-observationCode-san-kpi.md) · used by `alcohol-status-recorded`, `k363-calprotectin-before-scopie`

Code sources: [od-alcohol-use](ObservationDefinition-od-alcohol-use.md), [od-calprotectin](ObservationDefinition-od-calprotectin.md)

Elements released: `code`, `value[x]`, `effective[x]`, `subject`

| | |
| :--- | :--- |
| [Alcoholgebruik](ObservationDefinition-od-alcohol-use.md) | value[x] is a CodeableConcept from alcohol-use-status (`observation-san-kpi-od-alcohol-use`) |
| [Calprotectine (feces)](ObservationDefinition-od-calprotectin.md) | value[x] is a Quantity in ug/g (`observation-san-kpi-od-calprotectin`) |

#### Procedure

Profile: [Procedure (SAN KPI)](StructureDefinition-procedure-san-kpi.md) · code value set: [Procedure code (SAN KPI)](ValueSet-procedureCode-san-kpi.md) · used by `k312-scopie`, `k363-calprotectin-before-scopie`

Code sources: [local-verrichting-codes-nza](ValueSet-local-verrichting-codes-nza.md)

Elements released: `code`, `performed[x]`, `subject`

#### Patient

Profile: [Patient (SAN KPI)](StructureDefinition-patient-san-kpi.md) · used by `ibd-dataset`

Elements released: `gender`, `birthDate`

-------

### External ValueSets

Maintained outside this repository and pinned here. `_syncValueSets.py check` compares each pin with its source.

| | | | |
| :--- | :--- | :--- | :--- |
| [careplan-category](http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.124.11.140--20240925070229) | decor | 2024-09-25T07:02:29 | downloaded in the build |
| [alcohol-use-status](http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.40.2.7.3.2--20200901000000) | decor | 2020-09-01T00:00:00 | downloaded in the build |
| [smoking-status-nts](http://snomed.info/sct?fhir_vs=ecl/%3C%3C%2077176002%20OR%20%3C%3C%208392000%20OR%20%3C%3C%208517006) | fhir | not versioned (implicit) | reference only (not downloadable) |

