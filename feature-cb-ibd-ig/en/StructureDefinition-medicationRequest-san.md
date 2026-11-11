# MedicationRequest (SAN) - SIM on FHIR v0.1.0

## Resource Profile: MedicationRequest (SAN) 

**Usages:**

* This Profile is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-medicationRequest-san.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-medicationRequest-san.csv), [Excel](../StructureDefinition-medicationRequest-san.xlsx), [Schematron](../StructureDefinition-medicationRequest-san.sch) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "medicationRequest-san",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/medicationRequest-san",
  "version" : "0.1.0",
  "name" : "MedicationRequestSan",
  "title" : "MedicationRequest (SAN)",
  "status" : "draft",
  "date" : "2026-11-11T17:43:54+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Generiek Santeon FHIR R4-profiel voor medicatieafspraken.\n\nGebaseerd op het Santeon Informatiemodel (SIMMedicatieAfspraak).\n\nDe-identificatie wordt uitgevoerd volgens afzonderlijke\ntransformatiespecificaties en is geen inherente\neigenschap van dit profiel.",
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
    "comment" : "Informatieve mapping van FHIR MedicationRequest naar SIM CSV-bronvelden."
  },
  {
    "identity" : "workflow",
    "uri" : "http://hl7.org/fhir/workflow",
    "name" : "Workflow Pattern"
  },
  {
    "identity" : "script10.6",
    "uri" : "http://ncpdp.org/SCRIPT10_6",
    "name" : "Mapping to NCPDP SCRIPT 10.6"
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
  "type" : "MedicationRequest",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/MedicationRequest",
  "derivation" : "constraint",
  "differential" : {
    "element" : [{
      "id" : "MedicationRequest",
      "path" : "MedicationRequest"
    },
    {
      "id" : "MedicationRequest.identifier",
      "path" : "MedicationRequest.identifier",
      "min" : 1,
      "max" : "1"
    },
    {
      "id" : "MedicationRequest.identifier.use",
      "path" : "MedicationRequest.identifier.use",
      "max" : "0"
    },
    {
      "id" : "MedicationRequest.identifier.system",
      "path" : "MedicationRequest.identifier.system",
      "mapping" : [{
        "identity" : "sim",
        "map" : "MedicatieAfspraak;MedicatieAfspraakID",
        "comment" : "Systeemdeel van de samengestelde identifier."
      }]
    },
    {
      "id" : "MedicationRequest.identifier.value",
      "path" : "MedicationRequest.identifier.value",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "MedicatieAfspraak;MedicatieAfspraakID",
        "comment" : "Waardedeel van de samengestelde identifier; hash."
      }]
    },
    {
      "id" : "MedicationRequest.status",
      "path" : "MedicationRequest.status",
      "mapping" : [{
        "identity" : "sim",
        "map" : "MedicatieAfspraak;MedicatieAfspraakStatus",
        "comment" : "Codes volgens MedicatieAfspraakStatusSysteem; overeenkomst met medicationrequest-status verifiëren."
      }]
    },
    {
      "id" : "MedicationRequest.medication[x]",
      "path" : "MedicationRequest.medication[x]",
      "slicing" : {
        "discriminator" : [{
          "type" : "type",
          "path" : "$this"
        }],
        "ordered" : false,
        "rules" : "open"
      }
    },
    {
      "id" : "MedicationRequest.medication[x]:medicationCodeableConcept",
      "path" : "MedicationRequest.medication[x]",
      "sliceName" : "medicationCodeableConcept",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }]
    },
    {
      "id" : "MedicationRequest.medication[x]:medicationCodeableConcept.coding.system",
      "path" : "MedicationRequest.medication[x].coding.system",
      "mapping" : [{
        "identity" : "sim",
        "map" : "MedicatieAfspraak;ProductCodeSysteem"
      }]
    },
    {
      "id" : "MedicationRequest.medication[x]:medicationCodeableConcept.coding.code",
      "path" : "MedicationRequest.medication[x].coding.code",
      "mapping" : [{
        "identity" : "sim",
        "map" : "MedicatieAfspraak;ProductCode"
      }]
    },
    {
      "id" : "MedicationRequest.medication[x]:medicationCodeableConcept.coding.display",
      "path" : "MedicationRequest.medication[x].coding.display",
      "mapping" : [{
        "identity" : "sim",
        "map" : "MedicatieAfspraak;ProductNaam"
      }]
    },
    {
      "id" : "MedicationRequest.subject",
      "path" : "MedicationRequest.subject",
      "type" : [{
        "code" : "Reference",
        "targetProfile" : ["https://ig.santeon.nl/sim-on-fhir/StructureDefinition/patient-san"]
      }],
      "mapping" : [{
        "identity" : "sim",
        "map" : "MedicatieAfspraak;Identificatienummer",
        "comment" : "Patient-reference op basis van patiëntidentifier."
      }]
    },
    {
      "id" : "MedicationRequest.dosageInstruction.timing.repeat.bounds[x]",
      "path" : "MedicationRequest.dosageInstruction.timing.repeat.bounds[x]",
      "slicing" : {
        "discriminator" : [{
          "type" : "type",
          "path" : "$this"
        }],
        "ordered" : false,
        "rules" : "open"
      }
    },
    {
      "id" : "MedicationRequest.dosageInstruction.timing.repeat.bounds[x]:boundsPeriod",
      "path" : "MedicationRequest.dosageInstruction.timing.repeat.bounds[x]",
      "sliceName" : "boundsPeriod",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Period"
      }]
    },
    {
      "id" : "MedicationRequest.dosageInstruction.timing.repeat.bounds[x]:boundsPeriod.start",
      "path" : "MedicationRequest.dosageInstruction.timing.repeat.bounds[x].start",
      "mapping" : [{
        "identity" : "sim",
        "map" : "MedicatieAfspraak;MedicationRequestDosageInstructionTimingRepeatBoundsPeriodStart"
      }]
    },
    {
      "id" : "MedicationRequest.dosageInstruction.route.coding.system",
      "path" : "MedicationRequest.dosageInstruction.route.coding.system",
      "mapping" : [{
        "identity" : "sim",
        "map" : "MedicatieAfspraak;ToedieningswegSystem"
      }]
    },
    {
      "id" : "MedicationRequest.dosageInstruction.route.coding.code",
      "path" : "MedicationRequest.dosageInstruction.route.coding.code",
      "mapping" : [{
        "identity" : "sim",
        "map" : "MedicatieAfspraak;ToedieningswegCode"
      }]
    },
    {
      "id" : "MedicationRequest.dosageInstruction.route.coding.display",
      "path" : "MedicationRequest.dosageInstruction.route.coding.display",
      "mapping" : [{
        "identity" : "sim",
        "map" : "MedicatieAfspraak;Toedieningsweg"
      }]
    }]
  }
}

```
