# CarePlan (SAN) - SIM on FHIR v0.1.0

## Resource Profile: CarePlan (SAN) 

**Usages:**

* This Profile is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-carePlan-san.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-carePlan-san.csv), [Excel](../StructureDefinition-carePlan-san.xlsx), [Schematron](../StructureDefinition-carePlan-san.sch) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "carePlan-san",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/carePlan-san",
  "version" : "0.1.0",
  "name" : "CarePlanSan",
  "title" : "CarePlan (SAN)",
  "status" : "draft",
  "date" : "2026-11-11T17:38:18+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Generiek Santeon FHIR R4-profiel voor zorgplannen, zoals deelname\naan een zorgpad of thuismonitoringprogramma.\n\nGebaseerd op het Santeon Informatiemodel (SIMCarePlan).\n\nDe-identificatie wordt uitgevoerd volgens afzonderlijke\ntransformatiespecificaties en is geen inherente\neigenschap van dit profiel.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "sim",
    "uri" : "urn:santeon:sim:csv",
    "name" : "Santeon Informatiemodel (SIM CSV)",
    "comment" : "Informatieve mapping van FHIR CarePlan naar SIM CSV-bronvelden."
  },
  {
    "identity" : "workflow",
    "uri" : "http://hl7.org/fhir/workflow",
    "name" : "Workflow Pattern"
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
    "identity" : "v2",
    "uri" : "http://hl7.org/v2",
    "name" : "HL7 v2 Mapping"
  }],
  "kind" : "resource",
  "abstract" : false,
  "type" : "CarePlan",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/CarePlan",
  "derivation" : "constraint",
  "differential" : {
    "element" : [{
      "id" : "CarePlan",
      "path" : "CarePlan"
    },
    {
      "id" : "CarePlan.identifier",
      "path" : "CarePlan.identifier",
      "min" : 1,
      "max" : "1"
    },
    {
      "id" : "CarePlan.identifier.use",
      "path" : "CarePlan.identifier.use",
      "max" : "0"
    },
    {
      "id" : "CarePlan.identifier.system",
      "path" : "CarePlan.identifier.system",
      "mapping" : [{
        "identity" : "sim",
        "map" : "CarePlan;CarePlanIdentifier",
        "comment" : "Systeemdeel van de samengestelde identifier."
      }]
    },
    {
      "id" : "CarePlan.identifier.value",
      "path" : "CarePlan.identifier.value",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "CarePlan;CarePlanIdentifier",
        "comment" : "Waardedeel van de samengestelde identifier; hash."
      }]
    },
    {
      "id" : "CarePlan.status",
      "path" : "CarePlan.status",
      "mapping" : [{
        "identity" : "sim",
        "map" : "CarePlan;CarePlanStatus"
      }]
    },
    {
      "id" : "CarePlan.intent",
      "path" : "CarePlan.intent",
      "mapping" : [{
        "identity" : "sim",
        "map" : "CarePlan;CarePlanIntent",
        "comment" : "Uitgeschakeld in het SIM LM (buiten IBD-scope)."
      }]
    },
    {
      "id" : "CarePlan.category.coding.system",
      "path" : "CarePlan.category.coding.system",
      "mapping" : [{
        "identity" : "sim",
        "map" : "CarePlan;CarePlanCategoryCodingSystem"
      }]
    },
    {
      "id" : "CarePlan.category.coding.code",
      "path" : "CarePlan.category.coding.code",
      "mapping" : [{
        "identity" : "sim",
        "map" : "CarePlan;CarePlanCategoryCodingCode"
      }]
    },
    {
      "id" : "CarePlan.category.coding.display",
      "path" : "CarePlan.category.coding.display",
      "mapping" : [{
        "identity" : "sim",
        "map" : "CarePlan;CarePlanCategoryCodingDisplay"
      }]
    },
    {
      "id" : "CarePlan.subject",
      "path" : "CarePlan.subject",
      "type" : [{
        "code" : "Reference",
        "targetProfile" : ["https://ig.santeon.nl/sim-on-fhir/StructureDefinition/patient-san"]
      }],
      "mapping" : [{
        "identity" : "sim",
        "map" : "CarePlan;Identificatienummer",
        "comment" : "Patient-reference op basis van patiëntidentifier."
      }]
    },
    {
      "id" : "CarePlan.period.start",
      "path" : "CarePlan.period.start",
      "mapping" : [{
        "identity" : "sim",
        "map" : "CarePlan;CarePlanPeriodStart"
      }]
    },
    {
      "id" : "CarePlan.period.end",
      "path" : "CarePlan.period.end",
      "mapping" : [{
        "identity" : "sim",
        "map" : "CarePlan;CarePlanPeriodEnd"
      }]
    }]
  }
}

```
