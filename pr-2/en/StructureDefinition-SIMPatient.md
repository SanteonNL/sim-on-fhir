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
  "date" : "2026-10-26T15:58:34+00:00",
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
    "uri" : "SIM CSV",
    "name" : "SIM CSV naar SIM Patient"
  },
  {
    "identity" : "SIMPatientToFHIR",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/patient-san",
    "name" : "SIM Patient naar FHIR"
  },
  {
    "identity" : "SIMPatientRelatedPersonToFHIR",
    "uri" : "http://hl7.org/fhir/StructureDefinition/RelatedPerson",
    "name" : "SIM Patient gerelateerde persoon naar FHIR"
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
        "identity" : "SIMPatientFromSIMCSV",
        "map" : "Patient;GeslachtCodeSysteem, Patient;GeslachtCode, Patient;GeslachtOmschrijving"
      },
      {
        "identity" : "SIMPatientToFHIR",
        "map" : "Patient.gender"
      }]
    },
    {
      "id" : "SIMPatient.Land",
      "path" : "SIMPatient.Land",
      "short" : "Land waar het adres zich bevindt",
      "definition" : "Land waar het adres zich bevindt",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Coding"
      }],
      "binding" : {
        "strength" : "extensible",
        "valueSet" : "http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.40.2.20.5.2--20200901000000"
      },
      "mapping" : [{
        "identity" : "SIMPatientFromSIMCSV",
        "map" : "Patient;LandSysteem, Patient;Land"
      },
      {
        "identity" : "SIMPatientToFHIR",
        "map" : "Patient.address.country",
        "comment" : "ISO 3166-code naar string"
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
        "map" : "Patient;Geboortedatum"
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
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "SIMPatientFromSIMCSV",
        "map" : "Patient;DatumOverlijden"
      },
      {
        "identity" : "SIMPatientToFHIR",
        "map" : "Patient.deceasedDateTime"
      }]
    },
    {
      "id" : "SIMPatient.DatumCheckStatusOverlijden",
      "path" : "SIMPatient.DatumCheckStatusOverlijden",
      "short" : "Datum waarop de overlijdensstatus is gecontroleerd",
      "definition" : "Datum waarop de overlijdensstatus is gecontroleerd",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "SIMPatientFromSIMCSV",
        "map" : "Patient;DatumCheckStatusOverlijden"
      }]
    },
    {
      "id" : "SIMPatient.GerelateerdePersoon",
      "path" : "SIMPatient.GerelateerdePersoon",
      "short" : "Gerelateerde persoon",
      "definition" : "Gerelateerde persoon",
      "min" : 0,
      "max" : "*",
      "type" : [{
        "code" : "BackboneElement"
      }]
    },
    {
      "id" : "SIMPatient.GerelateerdePersoon.Identificatienummer",
      "path" : "SIMPatient.GerelateerdePersoon.Identificatienummer",
      "short" : "Identificatienummer van de gerelateerde persoon",
      "definition" : "Identificatienummer van de gerelateerde persoon",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMPatientFromSIMCSV",
        "map" : "Patient;GerelateerdPersoonID"
      },
      {
        "identity" : "SIMPatientRelatedPersonToFHIR",
        "map" : "RelatedPerson.identifier"
      }]
    },
    {
      "id" : "SIMPatient.GerelateerdePersoon.Relatie",
      "path" : "SIMPatient.GerelateerdePersoon.Relatie",
      "short" : "Relatie tot de patiënt",
      "definition" : "Relatie tot de patiënt",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }],
      "binding" : {
        "strength" : "extensible",
        "valueSet" : "http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.40.2.3.1.1--20200901000000"
      },
      "mapping" : [{
        "identity" : "SIMPatientFromSIMCSV",
        "map" : "Patient;GerelateerdeRelatieSysteem, Patient;GerelateerdeRelatie"
      },
      {
        "identity" : "SIMPatientRelatedPersonToFHIR",
        "map" : "RelatedPerson.relationship"
      }]
    },
    {
      "id" : "SIMPatient.Naam",
      "path" : "SIMPatient.Naam",
      "short" : "Naam van de patiënt",
      "definition" : "Naam van de patiënt",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "BackboneElement"
      }]
    },
    {
      "id" : "SIMPatient.Naam.Initialen",
      "path" : "SIMPatient.Naam.Initialen",
      "short" : "Initialen van de patiënt",
      "definition" : "Initialen van de patiënt",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "string"
      }],
      "mapping" : [{
        "identity" : "SIMPatientFromSIMCSV",
        "map" : "Patient;PatientNameInitials"
      },
      {
        "identity" : "SIMPatientToFHIR",
        "map" : "Patient.name.given"
      }]
    },
    {
      "id" : "SIMPatient.Naam.Achternaam",
      "path" : "SIMPatient.Naam.Achternaam",
      "short" : "Achternaam van de patiënt",
      "definition" : "Achternaam van de patiënt",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "string"
      }],
      "mapping" : [{
        "identity" : "SIMPatientFromSIMCSV",
        "map" : "Patient;PatientNameFamilyName"
      },
      {
        "identity" : "SIMPatientToFHIR",
        "map" : "Patient.name.family"
      }]
    }]
  }
}

```
