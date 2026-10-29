// =====================================================================
// DefinitionModels.fsh
// Logical Models for KPI definitions and dataset definitions, plus the
// extension that lets an ObservationDefinition describe a panel.
//
// The definitions in this folder (ObservationDefinitions, ValueSets, KPI
// definitions, dataset definitions) are the SOURCE from which
// _generateFromDefinitions.py derives the profiles, the export Groups and
// the export Parameters. Nothing under input/fsh/generated/ is edited by hand.
// =====================================================================


// ---------------------------------------------------------------------
// Extension — a panel ObservationDefinition lists its component definitions
// (FHIR R4 ObservationDefinition has no component element; R5 has hasMember)
// ---------------------------------------------------------------------
Extension: ODComponent
Id: od-component
Title: "ObservationDefinition Component"
Description: "Points from a panel ObservationDefinition (e.g. blood pressure) to the ObservationDefinition of one of its components (e.g. systolic). The component's own code and unit become a constraint on Observation.component in the generated profile."
* ^status = #draft
* ^context[+].type = #element
* ^context[=].expression = "ObservationDefinition"
* value[x] only Reference(ObservationDefinition)


// ---------------------------------------------------------------------
// Logical Model — KPI definition
// A KPI definition is role-neutral: it only says which data (resource +
// FHIR search parameters) is needed. Whether a criterion acts as numerator,
// denominator or exclusion is decided downstream, not here.
// ---------------------------------------------------------------------
Logical: KpiDefinition
Parent: Base
Id: kpi-definition
Title: "KPI definition (model)"
Description: """
The data a KPI needs, expressed as a list of FHIR search criteria. The union of
all KPI definitions determines the model: the generated profiles accept exactly
the codes and elements the KPIs ask for, and the generated export request
selects exactly the records they filter on.

**Parameter values** are FHIR search values and may contain references that the
generator resolves:

| Reference | Resolves to |
|---|---|
| `{vs:<id>}` | canonical URL of the ValueSet with that id |
| `{od:<id>.code}` | `system|code` token(s) of the ObservationDefinition's code |
| `{od:<id>.unit}` | `system|code` of the ObservationDefinition's quantitative unit |
| `{od:<id>.valid}` | canonical URL of the ObservationDefinition's valid coded value set |
| `{vs:<canonical URL>}` | a ValueSet maintained elsewhere (for example ART-DECOR), referenced by canonical |

A value may also be a literal: `code:in=<canonical URL>` names an external value set, and
`type=system|code,system|code` lists codes inline. Literal codes on a `code` parameter of a
KPI criterion are added to the generated code value set.

Anything else in curly braces, such as `{period.start}`, is a placeholder that
the exporter fills in at run time.
"""
* ^status = #draft

* id 1..1 string "KPI identifier, e.g. 'k363-calprotectin-before-scopie'."
* version 1..1 string "Semantic version of this KPI definition."
* title 1..1 string "Human-readable name."
* description 0..1 markdown "What the KPI measures."

* criteria 1..* BackboneElement "The data the KPI needs."
* criteria.resource 1..1 code "FHIR resource type, e.g. Observation."
* criteria.param 0..* BackboneElement "A FHIR search parameter."
* criteria.param.name 1..1 string "Search parameter name including modifier, e.g. 'code:in'."
* criteria.param.value 1..1 string "Search value; may contain {vs:...} / {od:...} references and {placeholders}."
* criteria.element 0..* string "Elements of the resource that must be released, as ElementName (e.g. 'value[x]'). Drives _elements and bounds the generated profile."

* postFilter 0..* BackboneElement "Logic that cannot be expressed as a FHIR search and is applied after the query."
* postFilter.description 1..1 markdown "What is filtered or related, in words."
* postFilter.expression 0..1 string "Optional formalisation (FHIRPath or pseudo-code) evaluated by the exporter or the downstream calculation."


// ---------------------------------------------------------------------
// Logical Model — dataset definition
// One dataset per disease area. Becomes one export on one Group.
// ---------------------------------------------------------------------
Logical: DatasetDefinition
Parent: Base
Id: dataset-definition
Title: "Dataset definition (model)"
Description: """
One dataset (for example IBD or COPD): the cohort that forms the Group, and the
KPI definitions whose data the export releases. The generator resolves a dataset
into a `Group` (cohort as bulk-data `member-filter`s) and a `Parameters`
(`_type`, `_typeFilter`, `_elements`), and renders the whole definition as a
readable page.
"""
* ^status = #draft

* id 1..1 string "Dataset identifier, e.g. 'ibd'."
* version 1..1 string "Semantic version of this dataset definition."
* title 1..1 string "Human-readable name."
* description 0..1 markdown "What the dataset is for."

* cohort 1..1 BackboneElement "Who is in the Group."
* cohort.description 0..1 markdown "The cohort in words, including anything the criteria cannot express."
* cohort.criteria 1..* BackboneElement "A member filter. A patient must satisfy every criterion."
* cohort.criteria.resource 1..1 code "FHIR resource type the criterion searches."
* cohort.criteria.param 0..* BackboneElement "A FHIR search parameter."
* cohort.criteria.param.name 1..1 string "Search parameter name including modifier."
* cohort.criteria.param.value 1..1 string "Search value; same reference syntax as in a KPI definition."
* cohort.postFilter 0..* BackboneElement "Cohort logic that cannot be expressed as a FHIR search."
* cohort.postFilter.description 1..1 markdown "What is filtered, in words."
* cohort.postFilter.expression 0..1 string "Optional formalisation."

* kpi 1..* string "Id of a KpiDefinition whose data the export releases."

* include 0..* BackboneElement "Data the dataset releases that does not belong to any KPI, for example the Patient model with a filter on gender. Same shape as a KPI criterion."
* include.resource 1..1 code "FHIR resource type."
* include.param 0..* BackboneElement "A FHIR search parameter."
* include.param.name 1..1 string "Search parameter name including modifier."
* include.param.value 1..1 string "Search value; same reference syntax as in a KPI definition."
* include.element 0..* string "Elements of the resource that must be released, as ElementName. Drives _elements and bounds the generated profile."
