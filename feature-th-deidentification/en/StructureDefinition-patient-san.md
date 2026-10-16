# Patient (SAN) - SIM on FHIR v0.1.0

## Resource Profile: Patient (SAN) 

**Usages:**

* Refer to this Profile: [Condition (SAN)](StructureDefinition-condition-san.md), [EpisodeOfCare (SAN)](StructureDefinition-episodeOfCare-san.md), [Observation (SAN)](StructureDefinition-observation-san.md) and [Procedure (SAN)](StructureDefinition-procedure-san.md)

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
  "date" : "2026-10-16T14:43:24+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "**De-identificatie:**\n- identifier → hashed\n- birthDate →  dateshift",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "sim",
    "uri" : "http://hl7.org/v3",
    "name" : "Santeon Informatie Model",
    "comment" : "Mapping van Patient elementen naar SIM"
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
      "id" : "Patient.meta",
      "path" : "Patient.meta",
      "max" : "0"
    },
    {
      "id" : "Patient.implicitRules",
      "path" : "Patient.implicitRules",
      "max" : "0"
    },
    {
      "id" : "Patient.language",
      "path" : "Patient.language",
      "max" : "0"
    },
    {
      "id" : "Patient.text",
      "path" : "Patient.text",
      "max" : "0"
    },
    {
      "id" : "Patient.contained",
      "path" : "Patient.contained",
      "max" : "0"
    },
    {
      "id" : "Patient.extension",
      "path" : "Patient.extension",
      "max" : "0"
    },
    {
      "id" : "Patient.modifierExtension",
      "path" : "Patient.modifierExtension",
      "max" : "0"
    },
    {
      "id" : "Patient.identifier",
      "path" : "Patient.identifier",
      "min" : 1,
      "max" : "1"
    },
    {
      "id" : "Patient.identifier.use",
      "path" : "Patient.identifier.use",
      "max" : "0"
    },
    {
      "id" : "Patient.identifier.value",
      "path" : "Patient.identifier.value",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "Patient;Identificatienummer",
        "comment" : "hashed"
      }]
    },
    {
      "id" : "Patient.active",
      "path" : "Patient.active",
      "max" : "0"
    },
    {
      "id" : "Patient.name",
      "path" : "Patient.name",
      "max" : "0"
    },
    {
      "id" : "Patient.telecom",
      "path" : "Patient.telecom",
      "max" : "0"
    },
    {
      "id" : "Patient.gender",
      "path" : "Patient.gender",
      "max" : "0"
    },
    {
      "id" : "Patient.birthDate",
      "path" : "Patient.birthDate",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "Patient;Geboortedatum",
        "comment" : "dateshift"
      }]
    },
    {
      "id" : "Patient.deceased[x]",
      "path" : "Patient.deceased[x]",
      "max" : "0"
    },
    {
      "id" : "Patient.address",
      "path" : "Patient.address",
      "max" : "0"
    },
    {
      "id" : "Patient.maritalStatus",
      "path" : "Patient.maritalStatus",
      "max" : "0"
    },
    {
      "id" : "Patient.multipleBirth[x]",
      "path" : "Patient.multipleBirth[x]",
      "max" : "0"
    },
    {
      "id" : "Patient.photo",
      "path" : "Patient.photo",
      "max" : "0"
    },
    {
      "id" : "Patient.contact",
      "path" : "Patient.contact",
      "max" : "0"
    },
    {
      "id" : "Patient.communication",
      "path" : "Patient.communication",
      "max" : "0"
    },
    {
      "id" : "Patient.generalPractitioner",
      "path" : "Patient.generalPractitioner",
      "max" : "0"
    },
    {
      "id" : "Patient.managingOrganization",
      "path" : "Patient.managingOrganization",
      "max" : "0"
    },
    {
      "id" : "Patient.link",
      "path" : "Patient.link",
      "max" : "0"
    }]
  }
}

```
