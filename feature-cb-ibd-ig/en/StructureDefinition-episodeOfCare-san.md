# EpisodeOfCare (SAN) - SIM on FHIR v0.1.0

## Resource Profile: EpisodeOfCare (SAN) 

**Usages:**

* Refer to this Profile: [Encounter (SAN)](StructureDefinition-encounter-san.md)

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-episodeOfCare-san.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-episodeOfCare-san.csv), [Excel](../StructureDefinition-episodeOfCare-san.xlsx), [Schematron](../StructureDefinition-episodeOfCare-san.sch) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "episodeOfCare-san",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/episodeOfCare-san",
  "version" : "0.1.0",
  "name" : "EpisodeOfCareSan",
  "title" : "EpisodeOfCare (SAN)",
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
  "description" : "Generiek Santeon FHIR R4-profiel voor zorgepisodes.\n\nGebaseerd op het Santeon Informatiemodel (SIMDBC), waarbij\nnog moet worden vastgesteld hoe een DBC-subtraject zich\nverhoudt tot een FHIR EpisodeOfCare.\n\nDe-identificatie wordt uitgevoerd volgens afzonderlijke\ntransformatiespecificaties en is geen inherente\neigenschap van dit profiel.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "sim-dbc",
    "uri" : "urn:santeon:sim:csv",
    "name" : "Santeon Informatiemodel (SIM CSV)",
    "comment" : "Informatieve mapping van FHIR EpisodeOfCare naar SIM DBC-bronvelden."
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
  }],
  "kind" : "resource",
  "abstract" : false,
  "type" : "EpisodeOfCare",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/EpisodeOfCare",
  "derivation" : "constraint",
  "differential" : {
    "element" : [{
      "id" : "EpisodeOfCare",
      "path" : "EpisodeOfCare"
    },
    {
      "id" : "EpisodeOfCare.identifier.system",
      "path" : "EpisodeOfCare.identifier.system",
      "mapping" : [{
        "identity" : "sim-dbc",
        "map" : "DBC;SubtrajectNr",
        "comment" : "Systeemdeel van de samengestelde identifier."
      }]
    },
    {
      "id" : "EpisodeOfCare.identifier.value",
      "path" : "EpisodeOfCare.identifier.value",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim-dbc",
        "map" : "DBC;SubtrajectNr",
        "comment" : "Waardedeel van de samengestelde identifier; hash volgens ETL-specificatie."
      }]
    },
    {
      "id" : "EpisodeOfCare.type.coding.system",
      "path" : "EpisodeOfCare.type.coding.system",
      "mapping" : [{
        "identity" : "sim-dbc",
        "map" : "DBC;ZorgTypeCodeSysteem"
      }]
    },
    {
      "id" : "EpisodeOfCare.type.coding.code",
      "path" : "EpisodeOfCare.type.coding.code",
      "mapping" : [{
        "identity" : "sim-dbc",
        "map" : "DBC;ZorgTypeCode"
      }]
    },
    {
      "id" : "EpisodeOfCare.type.coding.display",
      "path" : "EpisodeOfCare.type.coding.display",
      "mapping" : [{
        "identity" : "sim-dbc",
        "map" : "DBC;ZorgTypeOmschrijving"
      }]
    },
    {
      "id" : "EpisodeOfCare.diagnosis.condition",
      "path" : "EpisodeOfCare.diagnosis.condition",
      "type" : [{
        "code" : "Reference",
        "targetProfile" : ["https://ig.santeon.nl/sim-on-fhir/StructureDefinition/condition-san"]
      }]
    },
    {
      "id" : "EpisodeOfCare.patient",
      "path" : "EpisodeOfCare.patient",
      "type" : [{
        "code" : "Reference",
        "targetProfile" : ["https://ig.santeon.nl/sim-on-fhir/StructureDefinition/patient-san"]
      }],
      "mapping" : [{
        "identity" : "sim-dbc",
        "map" : "DBC;Identificatienummer",
        "comment" : "Reference naar PatientSan op basis van patiëntidentifier."
      }]
    },
    {
      "id" : "EpisodeOfCare.period.start",
      "path" : "EpisodeOfCare.period.start",
      "mapping" : [{
        "identity" : "sim-dbc",
        "map" : "DBC;DBCOpeningsDatum",
        "comment" : "Alleen indien gelijk aan begin van de zorgepisode."
      }]
    },
    {
      "id" : "EpisodeOfCare.period.end",
      "path" : "EpisodeOfCare.period.end",
      "mapping" : [{
        "identity" : "sim-dbc",
        "map" : "DBC;DBCSluitingsDatum",
        "comment" : "Alleen indien gelijk aan einde van de zorgepisode."
      }]
    }]
  }
}

```
