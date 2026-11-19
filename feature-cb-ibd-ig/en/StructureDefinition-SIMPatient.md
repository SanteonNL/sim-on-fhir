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
  "date" : "2026-11-19T16:54:56+00:00",
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
    "name" : "SIM CSV",
    "comment" : "Herleidbaarheid van SIMPatient-elementen naar de oorspronkelijke SIM CSV-attributen."
  },
  {
    "identity" : "SIMPatientToGBB",
    "uri" : "http://ig.nictiz.nl/gbb/StructureDefinition/LogicalModelPatient",
    "name" : "Nictiz GBB-afsprakenmodel",
    "comment" : "Semantische vergelijking tussen SIMPatient en Nictiz GBB Patient (referentieversie 1.0.0-alpha.2), inclusief toelichting op verschillen. Legenda: 🟢 Beoordeeld, equivalent of compatibel zonder relevante afwijking; 🟡 Verschil vastgesteld of nader te beoordelen."
  },
  {
    "identity" : "SIMPatientToFHIR",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/patient-san",
    "name" : "Santeon FHIR PatientSan",
    "comment" : "Mapping van SIMPatient-elementen naar het Santeon FHIR-profiel PatientSan, inclusief harmonisatie met FHIR-standaarden en toelichting op benodigde conversies."
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
      "max" : "*",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMPatientFromSIMCSV",
        "map" : "Patient;Identificatienummer"
      },
      {
        "identity" : "SIMPatientToGBB",
        "map" : "Patient.patientIdentificationNumber",
        "comment" : "🟢 Equivalent: beide gebruiken Identifier met cardinaliteit 1..*."
      },
      {
        "identity" : "SIMPatientToFHIR",
        "map" : "Patient.identifier"
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
        "strength" : "required",
        "valueSet" : "http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.40.2.17.1.8--20200901000000"
      },
      "mapping" : [{
        "identity" : "SIMPatientToGBB",
        "map" : "Patient.gender",
        "comment" : "🟡 Verschil in terminologiebinding: SIM gebruikt GeslachtCodelijst ZIB 2020; GBB gebruikt FHIR AdministrativeGender."
      },
      {
        "identity" : "SIMPatientToFHIR",
        "map" : "Patient.gender",
        "comment" : "Terminologiemapping van SIM GeslachtCodelijst ZIB 2020 naar FHIR AdministrativeGender vereist."
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
        "identity" : "SIMPatientToGBB",
        "map" : "Patient.dateOfBirth",
        "comment" : "🟢 Equivalent: SIM date is compatibel met GBB dateTime op dagniveau."
      },
      {
        "identity" : "SIMPatientToFHIR",
        "map" : "Patient.birthDate"
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
        "identity" : "SIMPatientToGBB",
        "map" : "Patient.deceasedDateTime",
        "comment" : "🟡 Verschil in representatie: SIM gebruikt date; GBB ondersteunt dateTime en boolean."
      },
      {
        "identity" : "SIMPatientToFHIR",
        "map" : "Patient.deceased[x]",
        "comment" : "SIM date wordt gerepresenteerd als FHIR dateTime met dagprecisie. De boolean-variant wordt niet vanuit dit SIM-element gevuld."
      }]
    }]
  }
}

```
