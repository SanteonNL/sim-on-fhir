# Verrichting (model) - SIM on FHIR v0.1.0

## Logical Model: Verrichting (model) 

**Usages:**

* This Logical Model is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-SIMVerrichting.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-SIMVerrichting.csv), [Excel](../StructureDefinition-SIMVerrichting.xlsx) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "SIMVerrichting",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMVerrichting",
  "version" : "0.1.0",
  "name" : "SIMVerrichting",
  "title" : "Verrichting (model)",
  "status" : "draft",
  "date" : "2026-10-26T15:58:19+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Logisch model voor een verrichting binnen het Santeon Informatiemodel.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "SIMVerrichtingFromSIMCSV",
    "uri" : "SIM CSVs",
    "name" : "SIM CSVs"
  },
  {
    "identity" : "SIMVerrichtingToFHIR",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/procedure-san",
    "name" : "SIMVerrichting naar FHIR"
  }],
  "kind" : "logical",
  "abstract" : false,
  "type" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMVerrichting",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Base",
  "derivation" : "specialization",
  "differential" : {
    "element" : [{
      "id" : "SIMVerrichting",
      "path" : "SIMVerrichting",
      "short" : "Verrichting (model)",
      "definition" : "Logisch model voor een verrichting binnen het Santeon Informatiemodel."
    },
    {
      "id" : "SIMVerrichting.VerrichtingID",
      "path" : "SIMVerrichting.VerrichtingID",
      "short" : "Identificatie van de verrichting",
      "definition" : "Identificatie van de verrichting",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMVerrichtingFromSIMCSV",
        "map" : "VerrichtingID"
      },
      {
        "identity" : "SIMVerrichtingToFHIR",
        "map" : "Procedure.identifier"
      }]
    },
    {
      "id" : "SIMVerrichting.Patient",
      "path" : "SIMVerrichting.Patient",
      "short" : "Patiënt waarop de verrichting betrekking heeft",
      "definition" : "Patiënt waarop de verrichting betrekking heeft",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMPatient"
      }],
      "mapping" : [{
        "identity" : "SIMVerrichtingFromSIMCSV",
        "map" : "Identificatienummer"
      },
      {
        "identity" : "SIMVerrichtingToFHIR",
        "map" : "Procedure.subject"
      }]
    },
    {
      "id" : "SIMVerrichting.VerrichtingType",
      "path" : "SIMVerrichting.VerrichtingType",
      "short" : "Type verrichting",
      "definition" : "Type verrichting",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }],
      "constraint" : [{
        "key" : "validatie-verrichtingtype",
        "severity" : "error",
        "human" : "Valideert VerrichtingType tegen de bij het gebruikte codesysteem behorende ValueSet.",
        "expression" : "(VerrichtingType.coding.where(system = 'NZa-URI').exists() implies VerrichtingType.memberOf('canonical-URL-NZa-ValueSet')) and (VerrichtingType.coding.where(system = 'DHD-URI').exists() implies VerrichtingType.memberOf('canonical-URL-DHD-ValueSet')) and (VerrichtingType.coding.where(system = 'http://snomed.info/sct').exists() implies VerrichtingType.memberOf('canonical-URL-SNOMED-ValueSet'))",
        "source" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMVerrichting"
      }],
      "mapping" : [{
        "identity" : "SIMVerrichtingToFHIR",
        "map" : "Procedure.code"
      }]
    },
    {
      "id" : "SIMVerrichting.VerrichtingType.coding.system",
      "path" : "SIMVerrichting.VerrichtingType.coding.system",
      "mapping" : [{
        "identity" : "SIMVerrichtingFromSIMCSV",
        "map" : "VerrichtingTypeCodeSysteem"
      }]
    },
    {
      "id" : "SIMVerrichting.VerrichtingType.coding.code",
      "path" : "SIMVerrichting.VerrichtingType.coding.code",
      "mapping" : [{
        "identity" : "SIMVerrichtingFromSIMCSV",
        "map" : "VerrichtingTypeCode"
      }]
    },
    {
      "id" : "SIMVerrichting.VerrichtingType.coding.display",
      "path" : "SIMVerrichting.VerrichtingType.coding.display",
      "mapping" : [{
        "identity" : "SIMVerrichtingFromSIMCSV",
        "map" : "VerrichtingTypeOmschrijving"
      }]
    },
    {
      "id" : "SIMVerrichting.StartDatum",
      "path" : "SIMVerrichting.StartDatum",
      "short" : "Startdatum en -tijd van de verrichting",
      "definition" : "Startdatum en -tijd van de verrichting",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "SIMVerrichtingFromSIMCSV",
        "map" : "VerrichtingStartDatum"
      },
      {
        "identity" : "SIMVerrichtingToFHIR",
        "map" : "Procedure.performedPeriod.start"
      }]
    },
    {
      "id" : "SIMVerrichting.EindDatum",
      "path" : "SIMVerrichting.EindDatum",
      "short" : "Einddatum en -tijd van de verrichting",
      "definition" : "Einddatum en -tijd van de verrichting",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "SIMVerrichtingFromSIMCSV",
        "map" : "VerrichtingEindDatum"
      },
      {
        "identity" : "SIMVerrichtingToFHIR",
        "map" : "Procedure.performedPeriod.end"
      }]
    }]
  }
}

```
