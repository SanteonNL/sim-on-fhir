# MedicationRequest (SAN) - SIM on FHIR v0.1.0

## Resource Profile: MedicationRequest (SAN) 

**Usages:**

* This Profile is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-medicationRequest-san.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-medicationRequest-san.csv), [Excel](../StructureDefinition-medicationRequest-san.xlsx), [Schematron](../StructureDefinition-medicationRequest-san.sch) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "medicationRequest-san",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/medicationRequest-san",
  "version" : "0.1.0",
  "name" : "MedicationRequestSan",
  "title" : "MedicationRequest (SAN)",
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
  "description" : "Santeon FHIR R4-profiel voor medicatieafspraken, gebaseerd op het Santeon Informatiemodel (SIMMedicatieAfspraak) en afgeleid van FHIR R4 MedicationRequest.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "MedicationRequestSanFromSIMMedicatieAfspraak",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMMedicatieAfspraak",
    "name" : "SIM Logical Model SIMMedicatieAfspraak",
    "comment" : "Herkomst van de MedicationRequestSan-elementen in Medicatieafspraak (SIMMedicatieAfspraak), met de conversietoelichting. Afgeleid van SIMMedicatieAfspraakToFHIR; wijzigingen alleen daar aanbrengen."
  },
  {
    "identity" : "workflow",
    "uri" : "http://hl7.org/fhir/workflow",
    "name" : "Workflow Pattern"
  },
  {
    "identity" : "script10.6",
    "uri" : "http://ncpdp.org/SCRIPT10_6",
    "name" : "Mapping to NCPDP SCRIPT 10.6"
  },
  {
    "identity" : "rim",
    "uri" : "http://hl7.org/v3",
    "name" : "RIM Mapping"
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
  }],
  "kind" : "resource",
  "abstract" : false,
  "type" : "MedicationRequest",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/MedicationRequest",
  "derivation" : "constraint",
  "differential" : {
    "element" : [{
      "id" : "MedicationRequest",
      "path" : "MedicationRequest"
    },
    {
      "id" : "MedicationRequest.identifier",
      "path" : "MedicationRequest.identifier",
      "mapping" : [{
        "identity" : "MedicationRequestSanFromSIMMedicatieAfspraak",
        "map" : "SIMMedicatieAfspraak.MedicatieAfspraakID",
        "comment" : "Direct: samengestelde identifier (system|value); het waardedeel wordt gehasht volgens de ETL-specificatie."
      }]
    },
    {
      "id" : "MedicationRequest.status",
      "path" : "MedicationRequest.status",
      "mapping" : [{
        "identity" : "MedicationRequestSanFromSIMMedicatieAfspraak",
        "map" : "SIMMedicatieAfspraak.MedicatieAfspraakStatus",
        "comment" : "Datatypeconversie: SIM Coding naar FHIR code; codes volgens MedicatieAfspraakStatusSysteem, overeenkomst met FHIR medicationrequest-status verifiëren."
      }]
    },
    {
      "id" : "MedicationRequest.medication[x]",
      "path" : "MedicationRequest.medication[x]",
      "slicing" : {
        "discriminator" : [{
          "type" : "type",
          "path" : "$this"
        }],
        "ordered" : false,
        "rules" : "open"
      }
    },
    {
      "id" : "MedicationRequest.medication[x]:medicationCodeableConcept",
      "path" : "MedicationRequest.medication[x]",
      "sliceName" : "medicationCodeableConcept",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }],
      "mapping" : [{
        "identity" : "MedicationRequestSanFromSIMMedicatieAfspraak",
        "map" : "SIMMedicatieAfspraak.Product",
        "comment" : "Direct: SIM CodeableConcept wordt overgenomen als FHIR CodeableConcept. Het SIM documenteert medicationReference naar een Medication met deze code; keuze nog te maken."
      }]
    },
    {
      "id" : "MedicationRequest.subject",
      "path" : "MedicationRequest.subject",
      "mapping" : [{
        "identity" : "MedicationRequestSanFromSIMMedicatieAfspraak",
        "map" : "SIMMedicatieAfspraak.PatientIdentificatienummer",
        "comment" : "Structuurconversie: Patient-reference op basis van identificatienummer."
      }]
    },
    {
      "id" : "MedicationRequest.dosageInstruction.timing.repeat.bounds[x]",
      "path" : "MedicationRequest.dosageInstruction.timing.repeat.bounds[x]",
      "slicing" : {
        "discriminator" : [{
          "type" : "type",
          "path" : "$this"
        }],
        "ordered" : false,
        "rules" : "open"
      }
    },
    {
      "id" : "MedicationRequest.dosageInstruction.timing.repeat.bounds[x]:boundsPeriod",
      "path" : "MedicationRequest.dosageInstruction.timing.repeat.bounds[x]",
      "sliceName" : "boundsPeriod",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Period"
      }]
    },
    {
      "id" : "MedicationRequest.dosageInstruction.timing.repeat.bounds[x]:boundsPeriod.start",
      "path" : "MedicationRequest.dosageInstruction.timing.repeat.bounds[x].start",
      "mapping" : [{
        "identity" : "MedicationRequestSanFromSIMMedicatieAfspraak",
        "map" : "SIMMedicatieAfspraak.GebruiksperiodeStart",
        "comment" : "Direct: SIM dateTime wordt overgenomen als FHIR dateTime."
      }]
    },
    {
      "id" : "MedicationRequest.dosageInstruction.route",
      "path" : "MedicationRequest.dosageInstruction.route",
      "mapping" : [{
        "identity" : "MedicationRequestSanFromSIMMedicatieAfspraak",
        "map" : "SIMMedicatieAfspraak.Toedieningsweg",
        "comment" : "Direct: SIM CodeableConcept wordt overgenomen als FHIR CodeableConcept."
      }]
    }]
  }
}

```
