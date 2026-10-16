# Patiënt (model) - SIM on FHIR v0.1.0

## Logical Model: Patiënt (model) 

**Usages:**

* Use this Logical Model: [DBC/Zorgtraject (model)](StructureDefinition-SIMDBC.md), [Fasering (model)](StructureDefinition-SIMFasering.md) and [Verrichting (model)](StructureDefinition-SIMVerrichting.md)

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-SIMPatient.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-SIMPatient.csv), [Excel](../StructureDefinition-SIMPatient.xlsx) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "SIMPatient",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMPatient",
  "version" : "0.1.0",
  "name" : "SIMPatient",
  "title" : "Patiënt (model)",
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
  "description" : "Logisch model voor patiëntgegevens binnen het Santeon Informatiemodel.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "SIMPatientFromSIMCSV",
    "uri" : "SIM CSV",
    "name" : "SIM CSV"
  },
  {
    "identity" : "SIMPatientToFHIR",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/patient-san",
    "name" : "SIM Patient naar FHIR"
  }],
  "kind" : "logical",
  "abstract" : false,
  "type" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMPatient",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Base",
  "derivation" : "specialization",
  "differential" : {
    "element" : [{
      "id" : "SIMPatient",
      "path" : "SIMPatient",
      "short" : "Patiënt (model)",
      "definition" : "Logisch model voor patiëntgegevens binnen het Santeon Informatiemodel."
    },
    {
      "id" : "SIMPatient.Identificatienummer",
      "path" : "SIMPatient.Identificatienummer",
      "short" : "Identificatienummer van de patiënt",
      "definition" : "Identificatienummer van de patiënt",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMPatientFromSIMCSV",
        "map" : "Identificatienummer"
      },
      {
        "identity" : "SIMPatientToFHIR",
        "map" : "Patient.identifier"
      }]
    },
    {
      "id" : "SIMPatient.Geboortedatum",
      "path" : "SIMPatient.Geboortedatum",
      "short" : "Geboortedatum van de patiënt",
      "definition" : "Geboortedatum van de patiënt",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "date"
      }],
      "mapping" : [{
        "identity" : "SIMPatientFromSIMCSV",
        "map" : "Geboortedatum"
      },
      {
        "identity" : "SIMPatientToFHIR",
        "map" : "Patient.birthDate"
      }]
    }]
  }
}

```
