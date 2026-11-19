# Patient (SAN) - SIM on FHIR v0.1.0

## Resource Profile: Patient (SAN) 

**Usages:**

* Refer to this Profile: [CarePlan (SAN)](StructureDefinition-carePlan-san.md), [Condition (SAN)](StructureDefinition-condition-san.md), [Encounter (SAN)](StructureDefinition-encounter-san.md), [EpisodeOfCare (SAN)](StructureDefinition-episodeOfCare-san.md)... Show 4 more, [MedicationRequest (SAN)](StructureDefinition-medicationRequest-san.md), [Observation (SAN)](StructureDefinition-observation-san.md), [Procedure (SAN)](StructureDefinition-procedure-san.md) and [QuestionnaireResponse (SAN)](StructureDefinition-questionnaireResponse-san.md)

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-patient-san.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-patient-san.csv), [Excel](../StructureDefinition-patient-san.xlsx), [Schematron](../StructureDefinition-patient-san.sch) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "patient-san",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/patient-san",
  "version" : "0.1.0",
  "name" : "PatientSan",
  "title" : "Patient (SAN)",
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
  "description" : "Santeon FHIR R4-profiel voor patiëntgegevens, gebaseerd op het Santeon Informatiemodel (SIMPatient).",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "PatientSanToNlCore",
    "uri" : "http://ig.nictiz.nl/gbb/StructureDefinition/nl-core-Patient",
    "name" : "Nictiz nl-core-Patient",
    "comment" : "Technische vergelijking tussen PatientSan en Nictiz nl-core Patient (referentieversie 1.0.0-alpha.2), inclusief toelichting op verschillen in FHIR-elementen, cardinaliteiten, datatypes en terminologiebindingen. Legenda: 🟢 Beoordeeld, equivalent of compatibel zonder relevante afwijking; 🟡 Verschil vastgesteld of nader te beoordelen."
  },
  {
    "identity" : "rim",
    "uri" : "http://hl7.org/v3",
    "name" : "RIM Mapping"
  },
  {
    "identity" : "cda",
    "uri" : "http://hl7.org/v3/cda",
    "name" : "CDA (R2)"
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
  },
  {
    "identity" : "loinc",
    "uri" : "http://loinc.org",
    "name" : "LOINC code for the element"
  }],
  "kind" : "resource",
  "abstract" : false,
  "type" : "Patient",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Patient",
  "derivation" : "constraint",
  "differential" : {
    "element" : [{
      "id" : "Patient",
      "path" : "Patient"
    },
    {
      "id" : "Patient.identifier",
      "path" : "Patient.identifier",
      "min" : 1,
      "mapping" : [{
        "identity" : "PatientSanToNlCore",
        "map" : "Patient.identifier",
        "comment" : "🟡 Verschil in profilering: Nictiz definieert aanvullend een optionele BSN-slice 0..1."
      }]
    },
    {
      "id" : "Patient.identifier.value",
      "path" : "Patient.identifier.value",
      "min" : 1
    },
    {
      "id" : "Patient.gender",
      "path" : "Patient.gender",
      "binding" : {
        "strength" : "required",
        "valueSet" : "http://hl7.org/fhir/ValueSet/administrative-gender"
      },
      "mapping" : [{
        "identity" : "PatientSanToNlCore",
        "map" : "Patient.gender",
        "comment" : "🟢 Equivalent: beide gebruiken FHIR AdministrativeGender met required binding en cardinaliteit 0..1."
      }]
    },
    {
      "id" : "Patient.birthDate",
      "path" : "Patient.birthDate",
      "mapping" : [{
        "identity" : "PatientSanToNlCore",
        "map" : "Patient.birthDate",
        "comment" : "🟢 Equivalent: beide gebruiken FHIR date met cardinaliteit 0..1."
      }]
    },
    {
      "id" : "Patient.deceased[x]",
      "path" : "Patient.deceased[x]",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "PatientSanToNlCore",
        "map" : "Patient.deceasedDateTime",
        "comment" : "🟡 Verschil in representatie: PatientSan beperkt deceased[x] tot dateTime; nl-core ondersteunt daarnaast boolean."
      }]
    }]
  }
}

```
