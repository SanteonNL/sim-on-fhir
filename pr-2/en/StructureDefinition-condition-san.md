# Condition (SAN) - SIM on FHIR v0.1.0

## Resource Profile: Condition (SAN) 

**Usages:**

* This Profile is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-condition-san.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-condition-san.csv), [Excel](../StructureDefinition-condition-san.xlsx), [Schematron](../StructureDefinition-condition-san.sch) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "condition-san",
  "extension" : [{
    "url" : "http://hl7.org/fhir/StructureDefinition/structuredefinition-imposeProfile",
    "valueCanonical" : "http://hl7.eu/fhir/base/StructureDefinition/condition-eu-core"
  }],
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/condition-san",
  "version" : "0.1.0",
  "name" : "ConditionSan",
  "title" : "Condition (SAN)",
  "status" : "draft",
  "date" : "2026-11-25T16:23:55+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Santeon FHIR R4-profiel voor aandoeningen en diagnoses, afgeleid van Nictiz nl-core-Condition. SIMDBC is de beoogde bron voor de diagnosegegevens. De mapping vanuit het Santeon Informatiemodel is nog niet vastgelegd.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "requirements-Condition",
    "uri" : "http://ig.nictiz.nl/gbb/Requirements/Condition",
    "name" : "Information requirements for Condition"
  },
  {
    "identity" : "lm-Condition",
    "uri" : "http://ig.nictiz.nl/gbb/StructureDefinition/LogicalModelCondition",
    "name" : "Logical model for Condition"
  },
  {
    "identity" : "workflow",
    "uri" : "http://hl7.org/fhir/workflow",
    "name" : "Workflow Pattern"
  },
  {
    "identity" : "sct-concept",
    "uri" : "http://snomed.info/conceptdomain",
    "name" : "SNOMED CT Concept Domain Binding"
  },
  {
    "identity" : "v2",
    "uri" : "http://hl7.org/v2",
    "name" : "HL7 v2 Mapping"
  },
  {
    "identity" : "rim",
    "uri" : "http://hl7.org/v3",
    "name" : "RIM Mapping"
  },
  {
    "identity" : "w5",
    "uri" : "http://hl7.org/fhir/fivews",
    "name" : "FiveWs Pattern Mapping"
  },
  {
    "identity" : "sct-attr",
    "uri" : "http://snomed.org/attributebinding",
    "name" : "SNOMED CT Attribute Binding"
  }],
  "kind" : "resource",
  "abstract" : false,
  "type" : "Condition",
  "baseDefinition" : "http://ig.nictiz.nl/gbb/StructureDefinition/nl-core-Condition",
  "derivation" : "constraint",
  "differential" : {
    "element" : [{
      "id" : "Condition",
      "path" : "Condition"
    }]
  }
}

```
