# Fasering (model) - SIM on FHIR v0.1.0

## Logical Model: Fasering (model) 

**Usages:**

* This Logical Model is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-SIMFasering.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-SIMFasering.csv), [Excel](../StructureDefinition-SIMFasering.xlsx) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "SIMFasering",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMFasering",
  "version" : "0.1.0",
  "name" : "SIMFasering",
  "title" : "Fasering (model)",
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
  "description" : "Logisch model voor een procesfasering binnen het Santeon Informatiemodel, bijvoorbeeld faseringen rondom een operatie.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "SIMFaseringFromSIMCSV",
    "uri" : "SIM CSVs",
    "name" : "SIM CSVs"
  },
  {
    "identity" : "SIMFaseringToFHIR",
    "uri" : "http://hl7.org/fhir/StructureDefinition/EncounterSan",
    "name" : "FHIR"
  }],
  "kind" : "logical",
  "abstract" : false,
  "type" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMFasering",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Base",
  "derivation" : "specialization",
  "differential" : {
    "element" : [{
      "id" : "SIMFasering",
      "path" : "SIMFasering",
      "short" : "Fasering (model)",
      "definition" : "Logisch model voor een procesfasering binnen het Santeon Informatiemodel, bijvoorbeeld faseringen rondom een operatie."
    },
    {
      "id" : "SIMFasering.FaseringID",
      "path" : "SIMFasering.FaseringID",
      "short" : "Unieke identificatie van de fasering",
      "definition" : "Unieke identificatie van de fasering",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMFaseringToFHIR",
        "map" : "Encounter.identifier"
      }]
    },
    {
      "id" : "SIMFasering.FaseringID.system",
      "path" : "SIMFasering.FaseringID.system",
      "mapping" : [{
        "identity" : "SIMFaseringFromSIMCSV",
        "map" : "FaseringID_system"
      }]
    },
    {
      "id" : "SIMFasering.FaseringID.value",
      "path" : "SIMFasering.FaseringID.value",
      "mapping" : [{
        "identity" : "SIMFaseringFromSIMCSV",
        "map" : "FaseringID_value"
      }]
    },
    {
      "id" : "SIMFasering.ContactID",
      "path" : "SIMFasering.ContactID",
      "short" : "Identificatie van het contact waarbinnen de fasering plaatsvindt",
      "definition" : "Identificatie van het contact waarbinnen de fasering plaatsvindt",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMFaseringToFHIR",
        "map" : "Encounter.partOf"
      }]
    },
    {
      "id" : "SIMFasering.ContactID.system",
      "path" : "SIMFasering.ContactID.system",
      "mapping" : [{
        "identity" : "SIMFaseringFromSIMCSV",
        "map" : "ContactID_system"
      }]
    },
    {
      "id" : "SIMFasering.ContactID.value",
      "path" : "SIMFasering.ContactID.value",
      "mapping" : [{
        "identity" : "SIMFaseringFromSIMCSV",
        "map" : "ContactID_value"
      }]
    },
    {
      "id" : "SIMFasering.OperatieID",
      "path" : "SIMFasering.OperatieID",
      "short" : "Identificatie van de operatie waarop de fasering betrekking heeft",
      "definition" : "Identificatie van de operatie waarop de fasering betrekking heeft",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }]
    },
    {
      "id" : "SIMFasering.OperatieID.system",
      "path" : "SIMFasering.OperatieID.system",
      "mapping" : [{
        "identity" : "SIMFaseringFromSIMCSV",
        "map" : "OperatieID_system"
      }]
    },
    {
      "id" : "SIMFasering.OperatieID.value",
      "path" : "SIMFasering.OperatieID.value",
      "mapping" : [{
        "identity" : "SIMFaseringFromSIMCSV",
        "map" : "OperatieID_value"
      }]
    },
    {
      "id" : "SIMFasering.Patient",
      "path" : "SIMFasering.Patient",
      "short" : "Identificatie van de patiënt",
      "definition" : "Identificatie van de patiënt",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMPatient"
      }],
      "mapping" : [{
        "identity" : "SIMFaseringToFHIR",
        "map" : "Encounter.subject"
      }]
    },
    {
      "id" : "SIMFasering.Patient.Identificatienummer.system",
      "path" : "SIMFasering.Patient.Identificatienummer.system",
      "mapping" : [{
        "identity" : "SIMFaseringFromSIMCSV",
        "map" : "Identificatienummer_system"
      }]
    },
    {
      "id" : "SIMFasering.Patient.Identificatienummer.value",
      "path" : "SIMFasering.Patient.Identificatienummer.value",
      "mapping" : [{
        "identity" : "SIMFaseringFromSIMCSV",
        "map" : "Identificatienummer_value"
      }]
    },
    {
      "id" : "SIMFasering.FaseringType",
      "path" : "SIMFasering.FaseringType",
      "short" : "Type procesfasering",
      "definition" : "Type procesfasering",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }],
      "mapping" : [{
        "identity" : "SIMFaseringToFHIR",
        "map" : "Encounter.type"
      }]
    },
    {
      "id" : "SIMFasering.FaseringType.coding.system",
      "path" : "SIMFasering.FaseringType.coding.system",
      "mapping" : [{
        "identity" : "SIMFaseringFromSIMCSV",
        "map" : "FaseringCodeSysteem"
      }]
    },
    {
      "id" : "SIMFasering.FaseringType.coding.code",
      "path" : "SIMFasering.FaseringType.coding.code",
      "mapping" : [{
        "identity" : "SIMFaseringFromSIMCSV",
        "map" : "FaseringCode"
      }]
    },
    {
      "id" : "SIMFasering.FaseringType.coding.display",
      "path" : "SIMFasering.FaseringType.coding.display",
      "mapping" : [{
        "identity" : "SIMFaseringFromSIMCSV",
        "map" : "FaseringOmschrijving"
      }]
    },
    {
      "id" : "SIMFasering.BeginDatumTijd",
      "path" : "SIMFasering.BeginDatumTijd",
      "short" : "Datum en tijd waarop de fasering begint",
      "definition" : "Datum en tijd waarop de fasering begint",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "SIMFaseringFromSIMCSV",
        "map" : "FaseringBeginDatumTijd"
      },
      {
        "identity" : "SIMFaseringToFHIR",
        "map" : "Encounter.period.start"
      }]
    },
    {
      "id" : "SIMFasering.EindDatumTijd",
      "path" : "SIMFasering.EindDatumTijd",
      "short" : "Datum en tijd waarop de fasering eindigt",
      "definition" : "Datum en tijd waarop de fasering eindigt",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "SIMFaseringFromSIMCSV",
        "map" : "FaseringEindDatumTijd"
      },
      {
        "identity" : "SIMFaseringToFHIR",
        "map" : "Encounter.period.end"
      }]
    }]
  }
}

```
