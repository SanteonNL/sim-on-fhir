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
  "date" : "2026-10-29T22:11:52+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Generiek logisch model voor een procesfasering binnen het Santeon Informatiemodel, bijvoorbeeld faseringen rondom een operatie.",
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
    "uri" : "urn:santeon:sim:csv",
    "name" : "SIM CSV"
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
      "definition" : "Generiek logisch model voor een procesfasering binnen het Santeon Informatiemodel, bijvoorbeeld faseringen rondom een operatie."
    },
    {
      "id" : "SIMFasering.FaseringID",
      "path" : "SIMFasering.FaseringID",
      "short" : "Uniek ID (key) binnen patiënt per fasering",
      "definition" : "Uniek ID (key) binnen patiënt per fasering",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMFaseringFromSIMCSV",
        "map" : "Fasering;FaseringID"
      }]
    },
    {
      "id" : "SIMFasering.ContactID",
      "path" : "SIMFasering.ContactID",
      "short" : "Uniek ID (key) binnen patiënt per contact",
      "definition" : "Uniek ID (key) binnen patiënt per contact",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMFaseringFromSIMCSV",
        "map" : "Fasering;ContactID"
      }]
    },
    {
      "id" : "SIMFasering.OperatieID",
      "path" : "SIMFasering.OperatieID",
      "short" : "Uniek ID (key) binnen patiënt per OK",
      "definition" : "Uniek ID (key) binnen patiënt per OK",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMFaseringFromSIMCSV",
        "map" : "Fasering;OperatieID"
      }]
    },
    {
      "id" : "SIMFasering.PatientIdentificatienummer",
      "path" : "SIMFasering.PatientIdentificatienummer",
      "short" : "Identificatienummer van de patiënt",
      "definition" : "Identificatienummer van de patiënt",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMFaseringFromSIMCSV",
        "map" : "Fasering;Identificatienummer"
      }]
    },
    {
      "id" : "SIMFasering.FaseringType",
      "path" : "SIMFasering.FaseringType",
      "short" : "Type procesfasering",
      "definition" : "Type procesfasering",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }]
    },
    {
      "id" : "SIMFasering.FaseringType.coding.system",
      "path" : "SIMFasering.FaseringType.coding.system",
      "mapping" : [{
        "identity" : "SIMFaseringFromSIMCSV",
        "map" : "Fasering;FaseringCodeSysteem"
      }]
    },
    {
      "id" : "SIMFasering.FaseringType.coding.code",
      "path" : "SIMFasering.FaseringType.coding.code",
      "mapping" : [{
        "identity" : "SIMFaseringFromSIMCSV",
        "map" : "Fasering;FaseringCode"
      }]
    },
    {
      "id" : "SIMFasering.FaseringType.coding.display",
      "path" : "SIMFasering.FaseringType.coding.display",
      "mapping" : [{
        "identity" : "SIMFaseringFromSIMCSV",
        "map" : "Fasering;FaseringOmschrijving"
      }]
    },
    {
      "id" : "SIMFasering.BeginDatumTijd",
      "path" : "SIMFasering.BeginDatumTijd",
      "short" : "Datum en eventueel tijd waarop de fasering begint",
      "definition" : "Datum en eventueel tijd waarop de fasering begint",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "SIMFaseringFromSIMCSV",
        "map" : "Fasering;FaseringBeginDatumTijd"
      }]
    },
    {
      "id" : "SIMFasering.EindDatumTijd",
      "path" : "SIMFasering.EindDatumTijd",
      "short" : "Datum en eventueel tijd waarop de fasering eindigt",
      "definition" : "Datum en eventueel tijd waarop de fasering eindigt",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "SIMFaseringFromSIMCSV",
        "map" : "Fasering;FaseringEindDatumTijd"
      }]
    }]
  }
}

```
