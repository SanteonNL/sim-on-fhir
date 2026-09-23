# De-identification Ruleset - Santeon CarePlan Implementation Guide v0.1.0

## Logical Model: De-identification Ruleset 

 
A fully-resolved (effective) set of de-identification rules for one export. It carries the actual rules in force, with no reference to a base ruleset plus differences. Together with the export Parameters (which declare the released elements via _elements), it fully describes how an export is de-identified. 

**Usages:**

* This Logical Model is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.careplan.ig|current/StructureDefinition/StructureDefinition-deidentification-ruleset.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-deidentification-ruleset.csv), [Excel](../StructureDefinition-deidentification-ruleset.xlsx) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "deidentification-ruleset",
  "url" : "https://ig.santeon.nl/careplan/StructureDefinition/deidentification-ruleset",
  "version" : "0.1.0",
  "name" : "DeidentificationRuleset",
  "title" : "De-identification Ruleset",
  "status" : "draft",
  "date" : "2026-09-23T11:39:33+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "A fully-resolved (effective) set of de-identification rules for one export.\nIt carries the actual rules in force, with no reference to a base ruleset\nplus differences. Together with the export Parameters (which declare the\nreleased elements via _elements), it fully describes how an export is\nde-identified.",
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
  "type" : "https://ig.santeon.nl/careplan/StructureDefinition/deidentification-ruleset",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Base",
  "derivation" : "specialization",
  "differential" : {
    "element" : [{
      "id" : "deidentification-ruleset",
      "path" : "deidentification-ruleset",
      "short" : "De-identification Ruleset",
      "definition" : "A fully-resolved (effective) set of de-identification rules for one export.\nIt carries the actual rules in force, with no reference to a base ruleset\nplus differences. Together with the export Parameters (which declare the\nreleased elements via _elements), it fully describes how an export is\nde-identified."
    },
    {
      "id" : "deidentification-ruleset.id",
      "path" : "deidentification-ruleset.id",
      "short" : "Ruleset identifier, e.g. 'santeon-default'.",
      "definition" : "Ruleset identifier, e.g. 'santeon-default'.",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "string"
      }]
    },
    {
      "id" : "deidentification-ruleset.version",
      "path" : "deidentification-ruleset.version",
      "short" : "Semantic version of this ruleset.",
      "definition" : "Semantic version of this ruleset.",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "string"
      }]
    },
    {
      "id" : "deidentification-ruleset.url",
      "path" : "deidentification-ruleset.url",
      "short" : "Canonical URL, when this ruleset is a published resource.",
      "definition" : "Canonical URL, when this ruleset is a published resource.",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "uri"
      }]
    },
    {
      "id" : "deidentification-ruleset.rule",
      "path" : "deidentification-ruleset.rule",
      "short" : "The de-identification rules in force for this export.",
      "definition" : "The de-identification rules in force for this export.",
      "min" : 1,
      "max" : "*",
      "type" : [{
        "code" : "BackboneElement"
      }],
      "constraint" : [{
        "key" : "none-requires-reason",
        "severity" : "error",
        "human" : "Action 'none' must carry an exceptionReason and no transform parameters.",
        "expression" : "action = 'none' implies (exceptionReason.exists() and algorithm.empty() and maxDays.empty() and minAge.empty() and maxAge.empty())",
        "source" : "https://ig.santeon.nl/careplan/StructureDefinition/deidentification-ruleset"
      },
      {
        "key" : "hash-requires-algorithm",
        "severity" : "error",
        "human" : "Action 'hash' must specify algorithm and carry no date/age parameters.",
        "expression" : "action = 'hash' implies (algorithm.exists() and maxDays.empty() and minAge.empty() and maxAge.empty())",
        "source" : "https://ig.santeon.nl/careplan/StructureDefinition/deidentification-ruleset"
      },
      {
        "key" : "shift-requires-maxdays",
        "severity" : "error",
        "human" : "Action 'shift' must specify maxDays and carry no hash/age parameters.",
        "expression" : "action = 'shift' implies (maxDays.exists() and algorithm.empty() and minAge.empty() and maxAge.empty())",
        "source" : "https://ig.santeon.nl/careplan/StructureDefinition/deidentification-ruleset"
      },
      {
        "key" : "clampage-requires-bounds",
        "severity" : "error",
        "human" : "Action 'clamp-age' must specify both minAge and maxAge.",
        "expression" : "action = 'clamp-age' implies (minAge.exists() and maxAge.exists() and algorithm.empty() and maxDays.empty())",
        "source" : "https://ig.santeon.nl/careplan/StructureDefinition/deidentification-ruleset"
      }]
    },
    {
      "id" : "deidentification-ruleset.rule.path",
      "path" : "deidentification-ruleset.rule.path",
      "short" : "FHIRPath expression selecting the element(s) this rule applies to. A rule\n     covers the named element and every element beneath it.",
      "definition" : "FHIRPath expression selecting the element(s) this rule applies to. A rule\n     covers the named element and every element beneath it.",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "string"
      }]
    },
    {
      "id" : "deidentification-ruleset.rule.action",
      "path" : "deidentification-ruleset.rule.action",
      "short" : "The transformation to apply.",
      "definition" : "The transformation to apply.",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "code"
      }],
      "binding" : {
        "strength" : "required",
        "valueSet" : "https://ig.santeon.nl/careplan/ValueSet/deidentification-action-vs"
      }
    },
    {
      "id" : "deidentification-ruleset.rule.priority",
      "path" : "deidentification-ruleset.rule.priority",
      "short" : "Optional explicit ordering. For date elements the canonical order is\n     shift, then first-of-month, then clamp-age; priority is for finer control.",
      "definition" : "Optional explicit ordering. For date elements the canonical order is\n     shift, then first-of-month, then clamp-age; priority is for finer control.",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "integer"
      }]
    },
    {
      "id" : "deidentification-ruleset.rule.algorithm",
      "path" : "deidentification-ruleset.rule.algorithm",
      "short" : "Hash algorithm (hash only), e.g. 'hmac-sha256'.",
      "definition" : "Hash algorithm (hash only), e.g. 'hmac-sha256'.",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "code"
      }]
    },
    {
      "id" : "deidentification-ruleset.rule.propagateTo",
      "path" : "deidentification-ruleset.rule.propagateTo",
      "short" : "Reference paths rewritten with the same hash (hash on an id element),\n     e.g. '*.subject', '*.patient'.",
      "definition" : "Reference paths rewritten with the same hash (hash on an id element),\n     e.g. '*.subject', '*.patient'.",
      "min" : 0,
      "max" : "*",
      "type" : [{
        "code" : "string"
      }]
    },
    {
      "id" : "deidentification-ruleset.rule.maxDays",
      "path" : "deidentification-ruleset.rule.maxDays",
      "short" : "Maximum absolute date shift in days (shift only). Offset drawn from\n     +/- maxDays, never zero.",
      "definition" : "Maximum absolute date shift in days (shift only). Offset drawn from\n     +/- maxDays, never zero.",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "integer"
      }]
    },
    {
      "id" : "deidentification-ruleset.rule.minAge",
      "path" : "deidentification-ruleset.rule.minAge",
      "short" : "Lower age bound in years (clamp-age only).",
      "definition" : "Lower age bound in years (clamp-age only).",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "integer"
      }]
    },
    {
      "id" : "deidentification-ruleset.rule.maxAge",
      "path" : "deidentification-ruleset.rule.maxAge",
      "short" : "Upper age bound in years (clamp-age only).",
      "definition" : "Upper age bound in years (clamp-age only).",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "integer"
      }]
    },
    {
      "id" : "deidentification-ruleset.rule.exceptionReason",
      "path" : "deidentification-ruleset.rule.exceptionReason",
      "short" : "Justification. Required for action 'none'; required when 'clamp-age'\n     overrides the standard age range.",
      "definition" : "Justification. Required for action 'none'; required when 'clamp-age'\n     overrides the standard age range.",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "string"
      }]
    }]
  }
}

```
