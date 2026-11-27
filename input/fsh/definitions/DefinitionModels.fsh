// =====================================================================
// DefinitionModels.fsh
// Logical Models for data requirements and dataset definitions, plus the
// extension that lets an ObservationDefinition describe a panel.
//
// The definitions in this folder (ObservationDefinitions, ValueSets, data
// requirements, dataset definitions) are the SOURCE from which
// _generateFromDefinitions.py derives the per-dataset profiles, the export
// Groups and the export Parameters. Nothing under input/fsh/generated/ is
// edited by hand.
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
// Logical Model — data requirement
// A data requirement is role-neutral: it only says which data (resource +
// FHIR search parameters + elements) is needed, for a KPI, a PREM or any
// other use. Whether a criterion acts as numerator, denominator or
// exclusion is decided downstream, not here.
// ---------------------------------------------------------------------
Logical: DataRequirementDefinition
Parent: Base
Id: data-requirement-definition
Title: "Data requirement (model)"
Description: """
The data a use (a KPI, a questionnaire, an analysis) needs, expressed as a list of
FHIR search criteria and the elements to release. The data requirements of a dataset
determine its profiles: the generated profiles accept exactly the codes and elements
they ask for, and the generated export request selects exactly the records they
filter on. Every element asked for must be allowed by the hand-written SAN profile
the generated profile derives from; the generator warns when it is not.

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
requirement criterion are added to the generated code value set.

Anything else in curly braces, such as `{period.start}`, is a placeholder that
the exporter fills in at run time.
"""
* ^status = #draft

* id 1..1 string "Data requirement identifier, e.g. 'k363-calprotectin-before-scopie'."
* version 1..1 string "Semantic version of this data requirement."
* title 1..1 string "Human-readable name."
* description 0..1 markdown "What the data is needed for."

* requirement 0..* string "Id of another DataRequirementDefinition this one builds on. Its criteria and postFilters (and those of the requirements it builds on) are part of this one, so a KPI that needs the data of another KPI plus more lists that KPI here and adds only its own criteria. Cycles are not allowed."

* criteria 0..* BackboneElement "The data needed, in addition to that of the requirements it builds on. A data requirement has at least one criterion or builds on at least one other requirement."
* criteria.resource 1..1 code "FHIR resource type, e.g. Observation."
* criteria.param 0..* BackboneElement "A FHIR search parameter."
* criteria.param.name 1..1 string "Search parameter name including modifier, e.g. 'code:in'."
* criteria.param.value 0..1 string "Search value; may contain {vs:...} / {od:...} references and {placeholders}. Either value or coding."
* criteria.param.coding 0..* Coding "Codes for a token parameter, one per line with their display, instead of a value. The generator joins them into the search value system|code,system|code and shows the displays on the dataset page and in the generated code value set."
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
One dataset (for example sb-ibd or sb-copd): the data requirement that selects the
patients of the Group, and the data requirements whose data the export releases. The
generator resolves a dataset into a `Group` (the criteria of the group requirement as
bulk-data `member-filter`s), a `Parameters`
(`_type`, `_typeFilter`, `_elements`) and one profile per resource type (derived from
the SAN profile), and renders the whole definition as a readable page.
"""
* ^status = #draft

* id 1..1 string "Dataset identifier, e.g. 'sb-ibd'. Prefixes the generated Group, Parameters and profiles."
* version 1..1 string "Semantic version of this dataset definition."
* title 1..1 string "Human-readable name."
* description 0..1 markdown "What the dataset is for."

* group 1..1 string "Id of the DataRequirementDefinition that selects who is in the Group. Each of its criteria becomes a member-filter (a patient must satisfy every criterion); its description and postFilters describe the Group. Its elements are not used."

* requirement 1..* string "Id of a DataRequirementDefinition whose data the export releases."

* include 0..* BackboneElement "Data the dataset releases that does not belong to any data requirement, for example the Patient model with a filter on gender. Same shape as a requirement criterion."
* include.resource 1..1 code "FHIR resource type."
* include.param 0..* BackboneElement "A FHIR search parameter."
* include.param.name 1..1 string "Search parameter name including modifier."
* include.param.value 0..1 string "Search value; same reference syntax as in a data requirement. Either value or coding."
* include.param.coding 0..* Coding "Codes with their display instead of a value, as in a data requirement."
* include.element 0..* string "Elements of the resource that must be released, as ElementName. Drives _elements and bounds the generated profile."
