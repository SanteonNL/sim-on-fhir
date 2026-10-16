# KPI definition (model) - SIM on FHIR v0.1.0

## Logical Model: KPI definition (model) 

**Usages:**

* This Logical Model is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-kpi-definition.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-kpi-definition.csv), [Excel](../StructureDefinition-kpi-definition.xlsx) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "kpi-definition",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/kpi-definition",
  "version" : "0.1.0",
  "name" : "KpiDefinition",
  "title" : "KPI definition (model)",
  "status" : "draft",
  "date" : "2026-10-16T14:28:20+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "The data a KPI needs, expressed as a list of FHIR search criteria. The union of\nall KPI definitions determines the model: the generated profiles accept exactly\nthe codes and elements the KPIs ask for, and the generated export request\nselects exactly the records they filter on.\n\n**Parameter values** are FHIR search values and may contain references that the\ngenerator resolves:\n\n| Reference | Resolves to |\n|---|---|\n| `{vs:<id>}` | canonical URL of the ValueSet with that id |\n| `{od:<id>.code}` | `system|code` token(s) of the ObservationDefinition's code |\n| `{od:<id>.unit}` | `system|code` of the ObservationDefinition's quantitative unit |\n| `{od:<id>.valid}` | canonical URL of the ObservationDefinition's valid coded value set |\n| `{vs:<canonical URL>}` | a ValueSet maintained elsewhere (for example ART-DECOR), referenced by canonical |\n\nA value may also be a literal: `code:in=<canonical URL>` names an external value set, and\n`type=system|code,system|code` lists codes inline. Literal codes on a `code` parameter of a\nKPI criterion are added to the generated code value set.\n\nAnything else in curly braces, such as `{period.start}`, is a placeholder that\nthe exporter fills in at run time.",
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
  "type" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/kpi-definition",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Base",
  "derivation" : "specialization",
  "differential" : {
    "element" : [{
      "id" : "kpi-definition",
      "path" : "kpi-definition",
      "short" : "KPI definition (model)",
      "definition" : "The data a KPI needs, expressed as a list of FHIR search criteria. The union of\nall KPI definitions determines the model: the generated profiles accept exactly\nthe codes and elements the KPIs ask for, and the generated export request\nselects exactly the records they filter on.\n\n**Parameter values** are FHIR search values and may contain references that the\ngenerator resolves:\n\n| Reference | Resolves to |\n|---|---|\n| `{vs:<id>}` | canonical URL of the ValueSet with that id |\n| `{od:<id>.code}` | `system|code` token(s) of the ObservationDefinition's code |\n| `{od:<id>.unit}` | `system|code` of the ObservationDefinition's quantitative unit |\n| `{od:<id>.valid}` | canonical URL of the ObservationDefinition's valid coded value set |\n| `{vs:<canonical URL>}` | a ValueSet maintained elsewhere (for example ART-DECOR), referenced by canonical |\n\nA value may also be a literal: `code:in=<canonical URL>` names an external value set, and\n`type=system|code,system|code` lists codes inline. Literal codes on a `code` parameter of a\nKPI criterion are added to the generated code value set.\n\nAnything else in curly braces, such as `{period.start}`, is a placeholder that\nthe exporter fills in at run time."
    },
    {
      "id" : "kpi-definition.id",
      "path" : "kpi-definition.id",
      "short" : "KPI identifier, e.g. 'k363-calprotectin-before-scopie'.",
      "definition" : "KPI identifier, e.g. 'k363-calprotectin-before-scopie'.",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "string"
      }]
    },
    {
      "id" : "kpi-definition.version",
      "path" : "kpi-definition.version",
      "short" : "Semantic version of this KPI definition.",
      "definition" : "Semantic version of this KPI definition.",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "string"
      }]
    },
    {
      "id" : "kpi-definition.title",
      "path" : "kpi-definition.title",
      "short" : "Human-readable name.",
      "definition" : "Human-readable name.",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "string"
      }]
    },
    {
      "id" : "kpi-definition.description",
      "path" : "kpi-definition.description",
      "short" : "What the KPI measures.",
      "definition" : "What the KPI measures.",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "markdown"
      }]
    },
    {
      "id" : "kpi-definition.criteria",
      "path" : "kpi-definition.criteria",
      "short" : "The data the KPI needs.",
      "definition" : "The data the KPI needs.",
      "min" : 1,
      "max" : "*",
      "type" : [{
        "code" : "BackboneElement"
      }]
    },
    {
      "id" : "kpi-definition.criteria.resource",
      "path" : "kpi-definition.criteria.resource",
      "short" : "FHIR resource type, e.g. Observation.",
      "definition" : "FHIR resource type, e.g. Observation.",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "code"
      }]
    },
    {
      "id" : "kpi-definition.criteria.param",
      "path" : "kpi-definition.criteria.param",
      "short" : "A FHIR search parameter.",
      "definition" : "A FHIR search parameter.",
      "min" : 0,
      "max" : "*",
      "type" : [{
        "code" : "BackboneElement"
      }]
    },
    {
      "id" : "kpi-definition.criteria.param.name",
      "path" : "kpi-definition.criteria.param.name",
      "short" : "Search parameter name including modifier, e.g. 'code:in'.",
      "definition" : "Search parameter name including modifier, e.g. 'code:in'.",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "string"
      }]
    },
    {
      "id" : "kpi-definition.criteria.param.value",
      "path" : "kpi-definition.criteria.param.value",
      "short" : "Search value; may contain {vs:...} / {od:...} references and {placeholders}.",
      "definition" : "Search value; may contain {vs:...} / {od:...} references and {placeholders}.",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "string"
      }]
    },
    {
      "id" : "kpi-definition.criteria.element",
      "path" : "kpi-definition.criteria.element",
      "short" : "Elements of the resource that must be released, as ElementName (e.g. 'value[x]'). Drives _elements and bounds the generated profile.",
      "definition" : "Elements of the resource that must be released, as ElementName (e.g. 'value[x]'). Drives _elements and bounds the generated profile.",
      "min" : 0,
      "max" : "*",
      "type" : [{
        "code" : "string"
      }]
    },
    {
      "id" : "kpi-definition.postFilter",
      "path" : "kpi-definition.postFilter",
      "short" : "Logic that cannot be expressed as a FHIR search and is applied after the query.",
      "definition" : "Logic that cannot be expressed as a FHIR search and is applied after the query.",
      "min" : 0,
      "max" : "*",
      "type" : [{
        "code" : "BackboneElement"
      }]
    },
    {
      "id" : "kpi-definition.postFilter.description",
      "path" : "kpi-definition.postFilter.description",
      "short" : "What is filtered or related, in words.",
      "definition" : "What is filtered or related, in words.",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "markdown"
      }]
    },
    {
      "id" : "kpi-definition.postFilter.expression",
      "path" : "kpi-definition.postFilter.expression",
      "short" : "Optional formalisation (FHIRPath or pseudo-code) evaluated by the exporter or the downstream calculation.",
      "definition" : "Optional formalisation (FHIRPath or pseudo-code) evaluated by the exporter or the downstream calculation.",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "string"
      }]
    }]
  }
}

```
