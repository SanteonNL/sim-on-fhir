# Medicatieafspraak (model) - SIM on FHIR v0.1.0

## Logical Model: Medicatieafspraak (model) 

**Usages:**

* This Logical Model is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-SIMMedicatieAfspraak.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-SIMMedicatieAfspraak.csv), [Excel](../StructureDefinition-SIMMedicatieAfspraak.xlsx) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "SIMMedicatieAfspraak",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMMedicatieAfspraak",
  "version" : "0.1.0",
  "name" : "SIMMedicatieAfspraak",
  "title" : "Medicatieafspraak (model)",
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
  "description" : "Generiek logisch model voor een medicatieafspraak binnen het Santeon Informatiemodel.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "SIMMedicatieAfspraakFromSIMCSV",
    "uri" : "urn:santeon:sim:csv",
    "name" : "SIM CSV"
  },
  {
    "identity" : "SIMMedicatieAfspraakToFHIR",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/medicationRequest-san",
    "name" : "SIMMedicatieAfspraak naar FHIR"
  }],
  "kind" : "logical",
  "abstract" : false,
  "type" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMMedicatieAfspraak",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Base",
  "derivation" : "specialization",
  "differential" : {
    "element" : [{
      "id" : "SIMMedicatieAfspraak",
      "path" : "SIMMedicatieAfspraak",
      "short" : "Medicatieafspraak (model)",
      "definition" : "Generiek logisch model voor een medicatieafspraak binnen het Santeon Informatiemodel."
    },
    {
      "id" : "SIMMedicatieAfspraak.MedicatieAfspraakID",
      "path" : "SIMMedicatieAfspraak.MedicatieAfspraakID",
      "short" : "Uniek ID (key) binnen patiënt per medicatieafspraak",
      "definition" : "Uniek ID (key) binnen patiënt per medicatieafspraak",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMMedicatieAfspraakFromSIMCSV",
        "map" : "MedicatieAfspraak;MedicatieAfspraakID"
      },
      {
        "identity" : "SIMMedicatieAfspraakToFHIR",
        "map" : "MedicationRequest.identifier"
      }]
    },
    {
      "id" : "SIMMedicatieAfspraak.PatientIdentificatienummer",
      "path" : "SIMMedicatieAfspraak.PatientIdentificatienummer",
      "short" : "Identificatienummer van de patiënt",
      "definition" : "Identificatienummer van de patiënt",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMMedicatieAfspraakFromSIMCSV",
        "map" : "MedicatieAfspraak;Identificatienummer"
      },
      {
        "identity" : "SIMMedicatieAfspraakToFHIR",
        "map" : "MedicationRequest.subject",
        "comment" : "Patient-reference op basis van identificatienummer."
      }]
    },
    {
      "id" : "SIMMedicatieAfspraak.Product",
      "path" : "SIMMedicatieAfspraak.Product",
      "short" : "Voorgeschreven geneesmiddel",
      "definition" : "Voorgeschreven geneesmiddel, gecodeerd met ATC. De IBD-indicatoren selecteren op ATC-code.",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }],
      "mapping" : [{
        "identity" : "SIMMedicatieAfspraakToFHIR",
        "map" : "MedicationRequest.medicationCodeableConcept",
        "comment" : "Het SIM documenteert medicationReference naar een Medication met deze code; keuze nog te maken."
      }]
    },
    {
      "id" : "SIMMedicatieAfspraak.Product.coding.system",
      "path" : "SIMMedicatieAfspraak.Product.coding.system",
      "mapping" : [{
        "identity" : "SIMMedicatieAfspraakFromSIMCSV",
        "map" : "MedicatieAfspraak;ProductCodeSysteem"
      }]
    },
    {
      "id" : "SIMMedicatieAfspraak.Product.coding.code",
      "path" : "SIMMedicatieAfspraak.Product.coding.code",
      "mapping" : [{
        "identity" : "SIMMedicatieAfspraakFromSIMCSV",
        "map" : "MedicatieAfspraak;ProductCode"
      }]
    },
    {
      "id" : "SIMMedicatieAfspraak.Product.coding.display",
      "path" : "SIMMedicatieAfspraak.Product.coding.display",
      "mapping" : [{
        "identity" : "SIMMedicatieAfspraakFromSIMCSV",
        "map" : "MedicatieAfspraak;ProductNaam"
      }]
    },
    {
      "id" : "SIMMedicatieAfspraak.Toedieningsweg",
      "path" : "SIMMedicatieAfspraak.Toedieningsweg",
      "short" : "Route waarlangs de medicatie wordt toegediend",
      "definition" : "Route waarlangs de medicatie wordt toegediend",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }],
      "mapping" : [{
        "identity" : "SIMMedicatieAfspraakToFHIR",
        "map" : "MedicationRequest.dosageInstruction.route"
      }]
    },
    {
      "id" : "SIMMedicatieAfspraak.Toedieningsweg.coding.system",
      "path" : "SIMMedicatieAfspraak.Toedieningsweg.coding.system",
      "mapping" : [{
        "identity" : "SIMMedicatieAfspraakFromSIMCSV",
        "map" : "MedicatieAfspraak;ToedieningswegSystem"
      }]
    },
    {
      "id" : "SIMMedicatieAfspraak.Toedieningsweg.coding.code",
      "path" : "SIMMedicatieAfspraak.Toedieningsweg.coding.code",
      "mapping" : [{
        "identity" : "SIMMedicatieAfspraakFromSIMCSV",
        "map" : "MedicatieAfspraak;ToedieningswegCode"
      }]
    },
    {
      "id" : "SIMMedicatieAfspraak.Toedieningsweg.coding.display",
      "path" : "SIMMedicatieAfspraak.Toedieningsweg.coding.display",
      "mapping" : [{
        "identity" : "SIMMedicatieAfspraakFromSIMCSV",
        "map" : "MedicatieAfspraak;Toedieningsweg"
      }]
    },
    {
      "id" : "SIMMedicatieAfspraak.Doseerduur",
      "path" : "SIMMedicatieAfspraak.Doseerduur",
      "short" : "Duur waarvoor de medicatie is voorgeschreven",
      "definition" : "Duur waarvoor de medicatie is voorgeschreven",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Duration"
      }]
    },
    {
      "id" : "SIMMedicatieAfspraak.Doseerduur.value",
      "path" : "SIMMedicatieAfspraak.Doseerduur.value",
      "mapping" : [{
        "identity" : "SIMMedicatieAfspraakFromSIMCSV",
        "map" : "MedicatieAfspraak;Doseerduur"
      }]
    },
    {
      "id" : "SIMMedicatieAfspraak.Doseerduur.code",
      "path" : "SIMMedicatieAfspraak.Doseerduur.code",
      "mapping" : [{
        "identity" : "SIMMedicatieAfspraakFromSIMCSV",
        "map" : "MedicatieAfspraak;DoseerDuurEenheid"
      }]
    },
    {
      "id" : "SIMMedicatieAfspraak.GebruiksperiodeStart",
      "path" : "SIMMedicatieAfspraak.GebruiksperiodeStart",
      "short" : "Datum en eventueel tijd waarop de gebruiksinstructie van deze medicatieafspraak ingaat",
      "definition" : "Datum en eventueel tijd waarop de gebruiksinstructie van deze medicatieafspraak ingaat",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "SIMMedicatieAfspraakFromSIMCSV",
        "map" : "MedicatieAfspraak;MedicationRequestDosageInstructionTimingRepeatBoundsPeriodStart"
      },
      {
        "identity" : "SIMMedicatieAfspraakToFHIR",
        "map" : "MedicationRequest.dosageInstruction.timing.repeat.boundsPeriod.start"
      }]
    },
    {
      "id" : "SIMMedicatieAfspraak.MedicatieAfspraakStatus",
      "path" : "SIMMedicatieAfspraak.MedicatieAfspraakStatus",
      "short" : "Status van de medicatieafspraak",
      "definition" : "Status van de medicatieafspraak",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Coding"
      }],
      "mapping" : [{
        "identity" : "SIMMedicatieAfspraakToFHIR",
        "map" : "MedicationRequest.status"
      }]
    },
    {
      "id" : "SIMMedicatieAfspraak.MedicatieAfspraakStatus.system",
      "path" : "SIMMedicatieAfspraak.MedicatieAfspraakStatus.system",
      "mapping" : [{
        "identity" : "SIMMedicatieAfspraakFromSIMCSV",
        "map" : "MedicatieAfspraak;MedicatieAfspraakStatusSysteem"
      }]
    },
    {
      "id" : "SIMMedicatieAfspraak.MedicatieAfspraakStatus.code",
      "path" : "SIMMedicatieAfspraak.MedicatieAfspraakStatus.code",
      "mapping" : [{
        "identity" : "SIMMedicatieAfspraakFromSIMCSV",
        "map" : "MedicatieAfspraak;MedicatieAfspraakStatus"
      }]
    },
    {
      "id" : "SIMMedicatieAfspraak.VoorschrijverSpecialisme",
      "path" : "SIMMedicatieAfspraak.VoorschrijverSpecialisme",
      "short" : "Medisch specialisme van de voorschrijver",
      "definition" : "Medisch specialisme van de voorschrijver",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }]
    },
    {
      "id" : "SIMMedicatieAfspraak.VoorschrijverSpecialisme.coding.system",
      "path" : "SIMMedicatieAfspraak.VoorschrijverSpecialisme.coding.system",
      "mapping" : [{
        "identity" : "SIMMedicatieAfspraakFromSIMCSV",
        "map" : "MedicatieAfspraak;Voorschrijver_SpecialismeSysteem"
      }]
    },
    {
      "id" : "SIMMedicatieAfspraak.VoorschrijverSpecialisme.coding.code",
      "path" : "SIMMedicatieAfspraak.VoorschrijverSpecialisme.coding.code",
      "mapping" : [{
        "identity" : "SIMMedicatieAfspraakFromSIMCSV",
        "map" : "MedicatieAfspraak;Voorschrijver_Specialisme"
      }]
    }]
  }
}

```
