# Dataset definition (model) - SIM on FHIR v0.1.0

## Logical Model: Dataset definition (model) 

**Usages:**

* This Logical Model is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-dataset-definition.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-dataset-definition.csv), [Excel](../StructureDefinition-dataset-definition.xlsx) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "dataset-definition",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/dataset-definition",
  "version" : "0.1.0",
  "name" : "DatasetDefinition",
  "title" : "Dataset definition (model)",
  "status" : "draft",
  "date" : "2026-10-16T14:43:24+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "One dataset (for example IBD or COPD): the cohort that forms the Group, and the\nKPI definitions whose data the export releases. The generator resolves a dataset\ninto a `Group` (cohort as bulk-data `member-filter`s) and a `Parameters`\n(`_type`, `_typeFilter`, `_elements`), and renders the whole definition as a\nreadable page.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "kind" : "logical",
  "abstract" : false,
  "type" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/dataset-definition",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Base",
  "derivation" : "specialization",
  "differential" : {
    "element" : [{
      "id" : "dataset-definition",
      "path" : "dataset-definition",
      "short" : "Dataset definition (model)",
      "definition" : "One dataset (for example IBD or COPD): the cohort that forms the Group, and the\nKPI definitions whose data the export releases. The generator resolves a dataset\ninto a `Group` (cohort as bulk-data `member-filter`s) and a `Parameters`\n(`_type`, `_typeFilter`, `_elements`), and renders the whole definition as a\nreadable page."
    },
    {
      "id" : "dataset-definition.id",
      "path" : "dataset-definition.id",
      "short" : "Dataset identifier, e.g. 'ibd'.",
      "definition" : "Dataset identifier, e.g. 'ibd'.",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "string"
      }]
    },
    {
      "id" : "dataset-definition.version",
      "path" : "dataset-definition.version",
      "short" : "Semantic version of this dataset definition.",
      "definition" : "Semantic version of this dataset definition.",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "string"
      }]
    },
    {
      "id" : "dataset-definition.title",
      "path" : "dataset-definition.title",
      "short" : "Human-readable name.",
      "definition" : "Human-readable name.",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "string"
      }]
    },
    {
      "id" : "dataset-definition.description",
      "path" : "dataset-definition.description",
      "short" : "What the dataset is for.",
      "definition" : "What the dataset is for.",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "markdown"
      }]
    },
    {
      "id" : "dataset-definition.cohort",
      "path" : "dataset-definition.cohort",
      "short" : "Who is in the Group.",
      "definition" : "Who is in the Group.",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "BackboneElement"
      }]
    },
    {
      "id" : "dataset-definition.cohort.description",
      "path" : "dataset-definition.cohort.description",
      "short" : "The cohort in words, including anything the criteria cannot express.",
      "definition" : "The cohort in words, including anything the criteria cannot express.",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "markdown"
      }]
    },
    {
      "id" : "dataset-definition.cohort.criteria",
      "path" : "dataset-definition.cohort.criteria",
      "short" : "A member filter. A patient must satisfy every criterion.",
      "definition" : "A member filter. A patient must satisfy every criterion.",
      "min" : 1,
      "max" : "*",
      "type" : [{
        "code" : "BackboneElement"
      }]
    },
    {
      "id" : "dataset-definition.cohort.criteria.resource",
      "path" : "dataset-definition.cohort.criteria.resource",
      "short" : "FHIR resource type the criterion searches.",
      "definition" : "FHIR resource type the criterion searches.",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "code"
      }]
    },
    {
      "id" : "dataset-definition.cohort.criteria.param",
      "path" : "dataset-definition.cohort.criteria.param",
      "short" : "A FHIR search parameter.",
      "definition" : "A FHIR search parameter.",
      "min" : 0,
      "max" : "*",
      "type" : [{
        "code" : "BackboneElement"
      }]
    },
    {
      "id" : "dataset-definition.cohort.criteria.param.name",
      "path" : "dataset-definition.cohort.criteria.param.name",
      "short" : "Search parameter name including modifier.",
      "definition" : "Search parameter name including modifier.",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "string"
      }]
    },
    {
      "id" : "dataset-definition.cohort.criteria.param.value",
      "path" : "dataset-definition.cohort.criteria.param.value",
      "short" : "Search value; same reference syntax as in a KPI definition.",
      "definition" : "Search value; same reference syntax as in a KPI definition.",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "string"
      }]
    },
    {
      "id" : "dataset-definition.cohort.postFilter",
      "path" : "dataset-definition.cohort.postFilter",
      "short" : "Cohort logic that cannot be expressed as a FHIR search.",
      "definition" : "Cohort logic that cannot be expressed as a FHIR search.",
      "min" : 0,
      "max" : "*",
      "type" : [{
        "code" : "BackboneElement"
      }]
    },
    {
      "id" : "dataset-definition.cohort.postFilter.description",
      "path" : "dataset-definition.cohort.postFilter.description",
      "short" : "What is filtered, in words.",
      "definition" : "What is filtered, in words.",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "markdown"
      }]
    },
    {
      "id" : "dataset-definition.cohort.postFilter.expression",
      "path" : "dataset-definition.cohort.postFilter.expression",
      "short" : "Optional formalisation.",
      "definition" : "Optional formalisation.",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "string"
      }]
    },
    {
      "id" : "dataset-definition.kpi",
      "path" : "dataset-definition.kpi",
      "short" : "Id of a KpiDefinition whose data the export releases.",
      "definition" : "Id of a KpiDefinition whose data the export releases.",
      "min" : 1,
      "max" : "*",
      "type" : [{
        "code" : "string"
      }]
    },
    {
      "id" : "dataset-definition.include",
      "path" : "dataset-definition.include",
      "short" : "Data the dataset releases that does not belong to any KPI, for example the Patient model with a filter on gender. Same shape as a KPI criterion.",
      "definition" : "Data the dataset releases that does not belong to any KPI, for example the Patient model with a filter on gender. Same shape as a KPI criterion.",
      "min" : 0,
      "max" : "*",
      "type" : [{
        "code" : "BackboneElement"
      }]
    },
    {
      "id" : "dataset-definition.include.resource",
      "path" : "dataset-definition.include.resource",
      "short" : "FHIR resource type.",
      "definition" : "FHIR resource type.",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "code"
      }]
    },
    {
      "id" : "dataset-definition.include.param",
      "path" : "dataset-definition.include.param",
      "short" : "A FHIR search parameter.",
      "definition" : "A FHIR search parameter.",
      "min" : 0,
      "max" : "*",
      "type" : [{
        "code" : "BackboneElement"
      }]
    },
    {
      "id" : "dataset-definition.include.param.name",
      "path" : "dataset-definition.include.param.name",
      "short" : "Search parameter name including modifier.",
      "definition" : "Search parameter name including modifier.",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "string"
      }]
    },
    {
      "id" : "dataset-definition.include.param.value",
      "path" : "dataset-definition.include.param.value",
      "short" : "Search value; same reference syntax as in a KPI definition.",
      "definition" : "Search value; same reference syntax as in a KPI definition.",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "string"
      }]
    },
    {
      "id" : "dataset-definition.include.element",
      "path" : "dataset-definition.include.element",
      "short" : "Elements of the resource that must be released, as ElementName. Drives _elements and bounds the generated profile.",
      "definition" : "Elements of the resource that must be released, as ElementName. Drives _elements and bounds the generated profile.",
      "min" : 0,
      "max" : "*",
      "type" : [{
        "code" : "string"
      }]
    }]
  }
}

```
