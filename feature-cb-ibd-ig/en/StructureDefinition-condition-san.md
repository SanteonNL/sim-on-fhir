# Condition (SAN) - SIM on FHIR v0.1.0

## Resource Profile: Condition (SAN) 

**Usages:**

* Refer to this Profile: [EpisodeOfCare (SAN)](StructureDefinition-episodeOfCare-san.md)

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-condition-san.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-condition-san.csv), [Excel](../StructureDefinition-condition-san.xlsx), [Schematron](../StructureDefinition-condition-san.sch) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "condition-san",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/condition-san",
  "version" : "0.1.0",
  "name" : "ConditionSan",
  "title" : "Condition (SAN)",
  "status" : "draft",
  "date" : "2026-10-16T14:27:57+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "**De-identificatie:**\n- identifier → hashed\n- subject → hashed",
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
    "uri" : "http://hl7.org/fhir/workflow",
    "name" : "Santeon Informatie Model",
    "comment" : "Mapping van Condition elementen naar SIM"
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
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Condition",
  "derivation" : "constraint",
  "differential" : {
    "element" : [{
      "id" : "Condition",
      "path" : "Condition"
    },
    {
      "id" : "Condition.meta",
      "path" : "Condition.meta",
      "max" : "0"
    },
    {
      "id" : "Condition.implicitRules",
      "path" : "Condition.implicitRules",
      "max" : "0"
    },
    {
      "id" : "Condition.language",
      "path" : "Condition.language",
      "max" : "0"
    },
    {
      "id" : "Condition.text",
      "path" : "Condition.text",
      "max" : "0"
    },
    {
      "id" : "Condition.contained",
      "path" : "Condition.contained",
      "max" : "0"
    },
    {
      "id" : "Condition.extension",
      "path" : "Condition.extension",
      "max" : "0"
    },
    {
      "id" : "Condition.modifierExtension",
      "path" : "Condition.modifierExtension",
      "max" : "0"
    },
    {
      "id" : "Condition.identifier",
      "path" : "Condition.identifier",
      "min" : 1,
      "max" : "1"
    },
    {
      "id" : "Condition.identifier.use",
      "path" : "Condition.identifier.use",
      "max" : "0"
    },
    {
      "id" : "Condition.identifier.value",
      "path" : "Condition.identifier.value",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "Probleem;ProbleemID",
        "comment" : "hashed"
      }]
    },
    {
      "id" : "Condition.clinicalStatus",
      "path" : "Condition.clinicalStatus",
      "max" : "0"
    },
    {
      "id" : "Condition.verificationStatus",
      "path" : "Condition.verificationStatus",
      "max" : "0"
    },
    {
      "id" : "Condition.category",
      "path" : "Condition.category",
      "max" : "0"
    },
    {
      "id" : "Condition.severity",
      "path" : "Condition.severity",
      "max" : "0"
    },
    {
      "id" : "Condition.code",
      "path" : "Condition.code",
      "min" : 1
    },
    {
      "id" : "Condition.code.coding",
      "path" : "Condition.code.coding",
      "min" : 1
    },
    {
      "id" : "Condition.code.coding.system",
      "path" : "Condition.code.coding.system",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "DBC;SpecialismeDiagnoseCodeSysteem"
      }]
    },
    {
      "id" : "Condition.code.coding.code",
      "path" : "Condition.code.coding.code",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "DBC;SpecialismeDiagnoseCode"
      }]
    },
    {
      "id" : "Condition.code.coding.display",
      "path" : "Condition.code.coding.display",
      "mapping" : [{
        "identity" : "sim",
        "map" : "DBC;SpecialismeDiagnoseOmschrijving"
      }]
    },
    {
      "id" : "Condition.bodySite",
      "path" : "Condition.bodySite",
      "max" : "0"
    },
    {
      "id" : "Condition.subject",
      "path" : "Condition.subject",
      "type" : [{
        "code" : "Reference",
        "targetProfile" : ["https://ig.santeon.nl/sim-on-fhir/StructureDefinition/patient-san"]
      }],
      "mapping" : [{
        "identity" : "sim",
        "map" : "Probleem;Identificatienummer",
        "comment" : "hashed"
      }]
    },
    {
      "id" : "Condition.encounter",
      "path" : "Condition.encounter",
      "max" : "0"
    },
    {
      "id" : "Condition.onset[x]",
      "path" : "Condition.onset[x]",
      "max" : "0"
    },
    {
      "id" : "Condition.abatement[x]",
      "path" : "Condition.abatement[x]",
      "max" : "0"
    },
    {
      "id" : "Condition.recordedDate",
      "path" : "Condition.recordedDate",
      "max" : "0"
    },
    {
      "id" : "Condition.recorder",
      "path" : "Condition.recorder",
      "max" : "0"
    },
    {
      "id" : "Condition.asserter",
      "path" : "Condition.asserter",
      "max" : "0"
    },
    {
      "id" : "Condition.stage",
      "path" : "Condition.stage",
      "max" : "0"
    },
    {
      "id" : "Condition.evidence",
      "path" : "Condition.evidence",
      "max" : "0"
    },
    {
      "id" : "Condition.note",
      "path" : "Condition.note",
      "max" : "0"
    }]
  }
}

```
