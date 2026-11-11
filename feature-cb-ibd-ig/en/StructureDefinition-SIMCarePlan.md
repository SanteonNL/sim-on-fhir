# Zorgplan (model) - SIM on FHIR v0.1.0

## Logical Model: Zorgplan (model) 

**Usages:**

* This Logical Model is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-SIMCarePlan.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-SIMCarePlan.csv), [Excel](../StructureDefinition-SIMCarePlan.xlsx) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "SIMCarePlan",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMCarePlan",
  "version" : "0.1.0",
  "name" : "SIMCarePlan",
  "title" : "Zorgplan (model)",
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
  "description" : "Generiek logisch model voor een zorgplan (deelname aan een zorgpad of thuismonitoringprogramma) binnen het Santeon Informatiemodel.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "SIMCarePlanFromSIMCSV",
    "uri" : "urn:santeon:sim:csv",
    "name" : "SIM CSV"
  },
  {
    "identity" : "SIMCarePlanToFHIR",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/carePlan-san",
    "name" : "SIMCarePlan naar FHIR"
  }],
  "kind" : "logical",
  "abstract" : false,
  "type" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMCarePlan",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Base",
  "derivation" : "specialization",
  "differential" : {
    "element" : [{
      "id" : "SIMCarePlan",
      "path" : "SIMCarePlan",
      "short" : "Zorgplan (model)",
      "definition" : "Generiek logisch model voor een zorgplan (deelname aan een zorgpad of thuismonitoringprogramma) binnen het Santeon Informatiemodel."
    },
    {
      "id" : "SIMCarePlan.CarePlanID",
      "path" : "SIMCarePlan.CarePlanID",
      "short" : "Uniek ID (key) van het zorgplan",
      "definition" : "Uniek ID (key) van het zorgplan",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMCarePlanFromSIMCSV",
        "map" : "CarePlan;CarePlanIdentifier"
      },
      {
        "identity" : "SIMCarePlanToFHIR",
        "map" : "CarePlan.identifier"
      }]
    },
    {
      "id" : "SIMCarePlan.PatientIdentificatienummer",
      "path" : "SIMCarePlan.PatientIdentificatienummer",
      "short" : "Identificatienummer van de patiënt",
      "definition" : "Identificatienummer van de patiënt",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMCarePlanFromSIMCSV",
        "map" : "CarePlan;Identificatienummer"
      },
      {
        "identity" : "SIMCarePlanToFHIR",
        "map" : "CarePlan.subject",
        "comment" : "Patient-reference op basis van identificatienummer."
      }]
    },
    {
      "id" : "SIMCarePlan.Status",
      "path" : "SIMCarePlan.Status",
      "short" : "Status van het zorgplan",
      "definition" : "Status van het zorgplan",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "code"
      }],
      "mapping" : [{
        "identity" : "SIMCarePlanFromSIMCSV",
        "map" : "CarePlan;CarePlanStatus"
      },
      {
        "identity" : "SIMCarePlanToFHIR",
        "map" : "CarePlan.status"
      }]
    },
    {
      "id" : "SIMCarePlan.Categorie",
      "path" : "SIMCarePlan.Categorie",
      "short" : "Zorgpad (programma) of onderliggend protocol waartoe het zorgplan behoort",
      "definition" : "Zorgpad (programma) of onderliggend protocol waartoe het zorgplan behoort",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }],
      "mapping" : [{
        "identity" : "SIMCarePlanToFHIR",
        "map" : "CarePlan.category"
      }]
    },
    {
      "id" : "SIMCarePlan.Categorie.coding.system",
      "path" : "SIMCarePlan.Categorie.coding.system",
      "mapping" : [{
        "identity" : "SIMCarePlanFromSIMCSV",
        "map" : "CarePlan;CarePlanCategoryCodingSystem"
      }]
    },
    {
      "id" : "SIMCarePlan.Categorie.coding.code",
      "path" : "SIMCarePlan.Categorie.coding.code",
      "mapping" : [{
        "identity" : "SIMCarePlanFromSIMCSV",
        "map" : "CarePlan;CarePlanCategoryCodingCode"
      }]
    },
    {
      "id" : "SIMCarePlan.Categorie.coding.display",
      "path" : "SIMCarePlan.Categorie.coding.display",
      "mapping" : [{
        "identity" : "SIMCarePlanFromSIMCSV",
        "map" : "CarePlan;CarePlanCategoryCodingDisplay"
      }]
    },
    {
      "id" : "SIMCarePlan.StartDatumTijd",
      "path" : "SIMCarePlan.StartDatumTijd",
      "short" : "Datum en eventueel tijd waarop het zorgplan ingaat (inclusiedatum)",
      "definition" : "Datum en eventueel tijd waarop het zorgplan ingaat (inclusiedatum)",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "SIMCarePlanFromSIMCSV",
        "map" : "CarePlan;CarePlanPeriodStart"
      },
      {
        "identity" : "SIMCarePlanToFHIR",
        "map" : "CarePlan.period.start"
      }]
    },
    {
      "id" : "SIMCarePlan.EindDatumTijd",
      "path" : "SIMCarePlan.EindDatumTijd",
      "short" : "Datum en eventueel tijd waarop het zorgplan eindigt",
      "definition" : "Datum en eventueel tijd waarop het zorgplan eindigt",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "SIMCarePlanFromSIMCSV",
        "map" : "CarePlan;CarePlanPeriodEnd"
      },
      {
        "identity" : "SIMCarePlanToFHIR",
        "map" : "CarePlan.period.end"
      }]
    }]
  }
}

```
