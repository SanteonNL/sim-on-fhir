# Patient (SAN) - SIM on FHIR v0.1.0

## Resource Profile: Patient (SAN) 

**Usages:**

* This Profile is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-patient-san.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-patient-san.csv), [Excel](../StructureDefinition-patient-san.xlsx), [Schematron](../StructureDefinition-patient-san.sch) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "patient-san",
  "extension" : [{
    "url" : "http://hl7.org/fhir/StructureDefinition/structuredefinition-imposeProfile",
    "valueCanonical" : "http://hl7.eu/fhir/base/StructureDefinition/patient-eu-core"
  }],
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/patient-san",
  "version" : "0.1.0",
  "name" : "PatientSan",
  "title" : "Patient (SAN)",
  "status" : "draft",
  "date" : "2026-11-25T16:23:55+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Santeon FHIR R4-profiel voor patiëntgegevens, gebaseerd op het Santeon Informatiemodel (SIMPatient) en afgeleid van Nictiz nl-core-Patient.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "PatientSanFromSIMPatient",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMPatient",
    "name" : "SIM Logical Model SIMPatient",
    "comment" : "Herkomst van de PatientSan-elementen in Patiënt (SIMPatient), met de conversietoelichting. Afgeleid van SIMPatientToFHIR; wijzigingen alleen daar aanbrengen."
  },
  {
    "identity" : "PatientSanToNlCore",
    "uri" : "http://ig.nictiz.nl/gbb/StructureDefinition/nl-core-Patient",
    "name" : "Nictiz nl-core-Patient",
    "comment" : "Technische delta-analyse tussen PatientSan en Nictiz nl-core-Patient (referentieversie 1.0.0-alpha.2). Legt per FHIR-element binnen de IBD-scope de uitkomst van de vergelijking vast. Equivalent betekent dat het element is beoordeeld en er geen relevant technisch verschil is vastgesteld. 🟡 markeert een vastgesteld verschil of een openstaande beoordeling."
  },
  {
    "identity" : "requirements-Patient",
    "uri" : "http://ig.nictiz.nl/gbb/Requirements/Patient",
    "name" : "Information requirements for Patient"
  },
  {
    "identity" : "lm-Patient",
    "uri" : "http://ig.nictiz.nl/gbb/StructureDefinition/LogicalModelPatient",
    "name" : "Logical model for Patient"
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
  "baseDefinition" : "http://ig.nictiz.nl/gbb/StructureDefinition/nl-core-Patient",
  "derivation" : "constraint",
  "differential" : {
    "element" : [{
      "id" : "Patient",
      "path" : "Patient"
    },
    {
      "id" : "Patient.identifier",
      "path" : "Patient.identifier",
      "mapping" : [{
        "identity" : "PatientSanFromSIMPatient",
        "map" : "SIMPatient.Identificatienummer",
        "comment" : "Direct: system en value worden behouden conform de afgesproken identificatiesystematiek."
      },
      {
        "identity" : "PatientSanToNlCore",
        "map" : "Patient.identifier",
        "comment" : "Equivalent"
      }]
    },
    {
      "id" : "Patient.gender",
      "path" : "Patient.gender",
      "mapping" : [{
        "identity" : "PatientSanFromSIMPatient",
        "map" : "SIMPatient.Geslacht",
        "comment" : "Terminologieconversie: SIM GeslachtCodelijst ZIB 2020 naar FHIR AdministrativeGender."
      },
      {
        "identity" : "PatientSanToNlCore",
        "map" : "Patient.gender",
        "comment" : "Equivalent"
      }]
    },
    {
      "id" : "Patient.birthDate",
      "path" : "Patient.birthDate",
      "mapping" : [{
        "identity" : "PatientSanFromSIMPatient",
        "map" : "SIMPatient.Geboortedatum",
        "comment" : "Direct: SIM date wordt overgenomen als FHIR date."
      },
      {
        "identity" : "PatientSanToNlCore",
        "map" : "Patient.birthDate",
        "comment" : "Equivalent"
      }]
    },
    {
      "id" : "Patient.deceased[x]:deceasedDateTime",
      "path" : "Patient.deceased[x]",
      "sliceName" : "deceasedDateTime",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "PatientSanFromSIMPatient",
        "map" : "SIMPatient.DatumOverlijden",
        "comment" : "Datatypeconversie: SIM date naar FHIR dateTime met dagprecisie; deceasedBoolean wordt vanuit dit SIM-element niet gevuld."
      },
      {
        "identity" : "PatientSanToNlCore",
        "map" : "Patient.deceasedDateTime",
        "comment" : "Equivalent"
      }]
    }]
  }
}

```
