# Patiënt (model) - SIM on FHIR v0.1.0

## Logical Model: Patiënt (model) 

**Usages:**

* This Logical Model is not used by any profiles in this Specification

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
  "date" : "2026-10-29T22:11:52+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Generiek logisch model voor patiëntgegevens binnen het Santeon Informatiemodel.",
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
    "uri" : "urn:santeon:sim:csv",
    "name" : "SIM CSV"
  },
  {
    "identity" : "SIMPatientToFHIR",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/patient-san",
    "name" : "SIMPatient naar FHIR"
  },
  {
    "identity" : "SIMPatientRelatedPersonToFHIR",
    "uri" : "http://hl7.org/fhir/StructureDefinition/RelatedPerson",
    "name" : "SIMPatient gerelateerde persoon naar FHIR"
  },
  {
    "identity" : "SIMPatientToGBB",
    "uri" : "http://ig.nictiz.nl/gbb/StructureDefinition/LogicalModelPatient",
    "name" : "SIMPatient naar Nictiz GBB Patient"
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
      "definition" : "Generiek logisch model voor patiëntgegevens binnen het Santeon Informatiemodel."
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
        "map" : "Patient;Identificatienummer"
      },
      {
        "identity" : "SIMPatientToFHIR",
        "map" : "Patient.identifier"
      },
      {
        "identity" : "SIMPatientRelatedPersonToFHIR",
        "map" : "RelatedPerson.patient",
        "comment" : "Reference naar de patiënt waarop de relatie betrekking heeft."
      },
      {
        "identity" : "SIMPatientToGBB",
        "map" : "patientIdentificationNumber"
      }]
    },
    {
      "id" : "SIMPatient.Geslacht",
      "path" : "SIMPatient.Geslacht",
      "short" : "Administratief geslacht van de patiënt",
      "definition" : "Administratief geslacht van de patiënt",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }],
      "binding" : {
        "strength" : "extensible",
        "valueSet" : "http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.40.2.17.1.8--20200901000000"
      },
      "mapping" : [{
        "identity" : "SIMPatientToFHIR",
        "map" : "Patient.gender",
        "comment" : "Terminologiemapping naar FHIR AdministrativeGender"
      },
      {
        "identity" : "SIMPatientToGBB",
        "map" : "gender"
      }]
    },
    {
      "id" : "SIMPatient.Geslacht.coding.system",
      "path" : "SIMPatient.Geslacht.coding.system",
      "mapping" : [{
        "identity" : "SIMPatientFromSIMCSV",
        "map" : "Patient;GeslachtCodeSysteem"
      }]
    },
    {
      "id" : "SIMPatient.Geslacht.coding.code",
      "path" : "SIMPatient.Geslacht.coding.code",
      "mapping" : [{
        "identity" : "SIMPatientFromSIMCSV",
        "map" : "Patient;GeslachtCode"
      }]
    },
    {
      "id" : "SIMPatient.Geslacht.coding.display",
      "path" : "SIMPatient.Geslacht.coding.display",
      "mapping" : [{
        "identity" : "SIMPatientFromSIMCSV",
        "map" : "Patient;GeslachtOmschrijving"
      }]
    },
    {
      "id" : "SIMPatient.Geboortedatum",
      "path" : "SIMPatient.Geboortedatum",
      "short" : "Geboortedatum van de patiënt",
      "definition" : "Geboortedatum van de patiënt",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "date"
      }],
      "mapping" : [{
        "identity" : "SIMPatientFromSIMCSV",
        "map" : "Patient;Geboortedatum"
      },
      {
        "identity" : "SIMPatientToFHIR",
        "map" : "Patient.birthDate"
      },
      {
        "identity" : "SIMPatientToGBB",
        "map" : "dateOfBirth",
        "comment" : "Conversie van date naar dateTime zonder tijdstip te verzinnen."
      }]
    },
    {
      "id" : "SIMPatient.DatumOverlijden",
      "path" : "SIMPatient.DatumOverlijden",
      "short" : "Datum waarop de patiënt is overleden",
      "definition" : "Datum waarop de patiënt is overleden",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "date"
      }],
      "mapping" : [{
        "identity" : "SIMPatientFromSIMCSV",
        "map" : "Patient;DatumOverlijden"
      },
      {
        "identity" : "SIMPatientToFHIR",
        "map" : "Patient.deceasedDateTime",
        "comment" : "Datum uit SIM omzetten naar FHIR dateTime, met behoud van de oorspronkelijke datumprecisie."
      },
      {
        "identity" : "SIMPatientToGBB",
        "map" : "deceasedDateTime"
      }]
    }]
  }
}

```
