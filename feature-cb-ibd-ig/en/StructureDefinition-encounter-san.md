# Encounter (SAN) - SIM on FHIR v0.1.0

## Resource Profile: Encounter (SAN) 

**Usages:**

* Refer to this Profile: [Procedure (SAN)](StructureDefinition-procedure-san.md)

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-encounter-san.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-encounter-san.csv), [Excel](../StructureDefinition-encounter-san.xlsx), [Schematron](../StructureDefinition-encounter-san.sch) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "encounter-san",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/encounter-san",
  "version" : "0.1.0",
  "name" : "EncounterSan",
  "title" : "Encounter (SAN)",
  "status" : "draft",
  "date" : "2026-10-29T22:11:52+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Generiek Santeon FHIR R4-profiel voor zorgcontacten.\n\nGebaseerd op het Santeon Informatiemodel (SIMContact).\n\nDe-identificatie wordt uitgevoerd volgens afzonderlijke\ntransformatiespecificaties en is geen inherente\neigenschap van dit profiel.",
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
    "comment" : "Informatieve mapping van FHIR Encounter naar SIM Contact-bronvelden."
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
  "type" : "Encounter",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Encounter",
  "derivation" : "constraint",
  "differential" : {
    "element" : [{
      "id" : "Encounter",
      "path" : "Encounter"
    },
    {
      "id" : "Encounter.identifier",
      "path" : "Encounter.identifier",
      "min" : 1,
      "max" : "1"
    },
    {
      "id" : "Encounter.identifier.system",
      "path" : "Encounter.identifier.system",
      "mapping" : [{
        "identity" : "sim",
        "map" : "Contact;ContactID",
        "comment" : "Systeemdeel van identifier_system|identifier_value."
      }]
    },
    {
      "id" : "Encounter.identifier.value",
      "path" : "Encounter.identifier.value",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "Contact;ContactID",
        "comment" : "Waardedeel van de identifier; hash volgens ETL-specificatie."
      }]
    },
    {
      "id" : "Encounter.class.system",
      "path" : "Encounter.class.system",
      "mapping" : [{
        "identity" : "sim",
        "map" : "Contact;ContactTypeCodeSysteem"
      }]
    },
    {
      "id" : "Encounter.class.code",
      "path" : "Encounter.class.code",
      "mapping" : [{
        "identity" : "sim",
        "map" : "Contact;ContactTypeCode"
      }]
    },
    {
      "id" : "Encounter.class.display",
      "path" : "Encounter.class.display",
      "mapping" : [{
        "identity" : "sim",
        "map" : "Contact;ContactTypeOmschrijving"
      }]
    },
    {
      "id" : "Encounter.priority",
      "path" : "Encounter.priority",
      "binding" : {
        "strength" : "example",
        "valueSet" : "http://terminology.hl7.org/ValueSet/v3-ActPriority"
      }
    },
    {
      "id" : "Encounter.priority.coding.system",
      "path" : "Encounter.priority.coding.system",
      "mapping" : [{
        "identity" : "sim",
        "map" : "Contact;EncounterPriorityCodingSystem"
      }]
    },
    {
      "id" : "Encounter.priority.coding.code",
      "path" : "Encounter.priority.coding.code",
      "mapping" : [{
        "identity" : "sim",
        "map" : "Contact;EncounterPriorityCodingCode"
      }]
    },
    {
      "id" : "Encounter.priority.coding.display",
      "path" : "Encounter.priority.coding.display",
      "mapping" : [{
        "identity" : "sim",
        "map" : "Contact;EncounterPriorityCodingDisplay"
      }]
    },
    {
      "id" : "Encounter.subject",
      "path" : "Encounter.subject",
      "min" : 1,
      "type" : [{
        "code" : "Reference",
        "targetProfile" : ["https://ig.santeon.nl/sim-on-fhir/StructureDefinition/patient-san"]
      }],
      "mapping" : [{
        "identity" : "sim",
        "map" : "Contact;Identificatienummer",
        "comment" : "Patient-reference op basis van de patiëntidentifier."
      }]
    },
    {
      "id" : "Encounter.episodeOfCare",
      "path" : "Encounter.episodeOfCare",
      "type" : [{
        "code" : "Reference",
        "targetProfile" : ["https://ig.santeon.nl/sim-on-fhir/StructureDefinition/episodeOfCare-san"]
      }],
      "mapping" : [{
        "identity" : "sim",
        "map" : "Contact;SubtrajectNr",
        "comment" : "Alleen indien DBC-subtraject als EpisodeOfCareSan wordt gerepresenteerd."
      }]
    },
    {
      "id" : "Encounter.appointment",
      "path" : "Encounter.appointment",
      "mapping" : [{
        "identity" : "sim",
        "map" : "Contact;AfspraakID",
        "comment" : "Reference naar Appointment op basis van AfspraakID."
      }]
    },
    {
      "id" : "Encounter.period.start",
      "path" : "Encounter.period.start",
      "mapping" : [{
        "identity" : "sim",
        "map" : "Contact;BeginDatumTijd",
        "comment" : "Dateshift volgens afzonderlijke transformatiespecificatie."
      }]
    },
    {
      "id" : "Encounter.period.end",
      "path" : "Encounter.period.end",
      "mapping" : [{
        "identity" : "sim",
        "map" : "Contact;EindDatumTijd",
        "comment" : "Dateshift volgens afzonderlijke transformatiespecificatie."
      }]
    },
    {
      "id" : "Encounter.hospitalization.dischargeDisposition.coding.system",
      "path" : "Encounter.hospitalization.dischargeDisposition.coding.system",
      "mapping" : [{
        "identity" : "sim",
        "map" : "Contact;BestemmingCodeSysteem",
        "comment" : "Alleen bij ontslagbestemming."
      }]
    },
    {
      "id" : "Encounter.hospitalization.dischargeDisposition.coding.code",
      "path" : "Encounter.hospitalization.dischargeDisposition.coding.code",
      "mapping" : [{
        "identity" : "sim",
        "map" : "Contact;BestemmingCode",
        "comment" : "Alleen bij ontslagbestemming."
      }]
    },
    {
      "id" : "Encounter.hospitalization.dischargeDisposition.coding.display",
      "path" : "Encounter.hospitalization.dischargeDisposition.coding.display",
      "mapping" : [{
        "identity" : "sim",
        "map" : "Contact;BestemmingOmschrijving",
        "comment" : "Alleen bij ontslagbestemming."
      }]
    }]
  }
}

```
