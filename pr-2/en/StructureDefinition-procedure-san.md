# Procedure (SAN) - SIM on FHIR v0.1.0

## Resource Profile: Procedure (SAN) 

**Usages:**

* This Profile is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-procedure-san.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-procedure-san.csv), [Excel](../StructureDefinition-procedure-san.xlsx), [Schematron](../StructureDefinition-procedure-san.sch) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "procedure-san",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/procedure-san",
  "version" : "0.1.0",
  "name" : "ProcedureSan",
  "title" : "Procedure (SAN)",
  "status" : "draft",
  "date" : "2026-11-19T16:55:06+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Generiek Santeon FHIR R4-profiel voor verrichtingen.\n\nGebaseerd op het Santeon Informatiemodel (SIMVerrichting).\n\nDe-identificatie wordt uitgevoerd volgens afzonderlijke\ntransformatiespecificaties en is geen inherente\neigenschap van dit profiel.",
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
    "uri" : "urn:santeon:sim:csv",
    "name" : "Santeon Informatiemodel (SIM CSV)",
    "comment" : "Informatieve mapping van het FHIR-profiel naar SIM CSV-bronvelden."
  },
  {
    "identity" : "ProcedureSanToNlCore",
    "uri" : "http://ig.nictiz.nl/gbb/StructureDefinition/nl-core-Procedure",
    "name" : "ProcedureSan naar Nictiz nl-core-Procedure",
    "comment" : "FHIR-profielvergelijking tussen ProcedureSan en Nictiz nl-core-Procedure (GBB 1.0.0-alpha.2), binnen de huidige IBD-scope."
  },
  {
    "identity" : "workflow",
    "uri" : "http://hl7.org/fhir/workflow",
    "name" : "Workflow Pattern"
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
  "type" : "Procedure",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Procedure",
  "derivation" : "constraint",
  "differential" : {
    "element" : [{
      "id" : "Procedure",
      "path" : "Procedure"
    },
    {
      "id" : "Procedure.meta",
      "path" : "Procedure.meta",
      "max" : "0"
    },
    {
      "id" : "Procedure.implicitRules",
      "path" : "Procedure.implicitRules",
      "max" : "0"
    },
    {
      "id" : "Procedure.language",
      "path" : "Procedure.language",
      "max" : "0"
    },
    {
      "id" : "Procedure.text",
      "path" : "Procedure.text",
      "max" : "0"
    },
    {
      "id" : "Procedure.contained",
      "path" : "Procedure.contained",
      "max" : "0"
    },
    {
      "id" : "Procedure.identifier",
      "path" : "Procedure.identifier",
      "min" : 1,
      "max" : "1",
      "mapping" : [{
        "identity" : "ProcedureSanToNlCore",
        "map" : "Procedure.identifier",
        "comment" : "Santeon 1..1; nl-core 0..*. Santeon strenger."
      }]
    },
    {
      "id" : "Procedure.identifier.use",
      "path" : "Procedure.identifier.use",
      "max" : "0",
      "mapping" : [{
        "identity" : "ProcedureSanToNlCore",
        "map" : "Procedure.identifier.use",
        "comment" : "Santeon 0..0; nl-core staat dit toe."
      }]
    },
    {
      "id" : "Procedure.identifier.system",
      "path" : "Procedure.identifier.system",
      "mapping" : [{
        "identity" : "sim",
        "map" : "Verrichting;VerrichtingID",
        "comment" : "Systeemdeel van identifier_system|identifier_value."
      },
      {
        "identity" : "ProcedureSanToNlCore",
        "map" : "Procedure.identifier.system",
        "comment" : "Identificatiesysteem behouden."
      }]
    },
    {
      "id" : "Procedure.identifier.value",
      "path" : "Procedure.identifier.value",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "Verrichting;VerrichtingID",
        "comment" : "Waardedeel; hashed."
      },
      {
        "identity" : "ProcedureSanToNlCore",
        "map" : "Procedure.identifier.value",
        "comment" : "Santeon vereist een waarde (1..1)."
      }]
    },
    {
      "id" : "Procedure.instantiatesCanonical",
      "path" : "Procedure.instantiatesCanonical",
      "max" : "0"
    },
    {
      "id" : "Procedure.instantiatesUri",
      "path" : "Procedure.instantiatesUri",
      "max" : "0"
    },
    {
      "id" : "Procedure.partOf",
      "path" : "Procedure.partOf",
      "max" : "0"
    },
    {
      "id" : "Procedure.status",
      "path" : "Procedure.status",
      "short" : "Status van de verrichting",
      "comment" : "Verplicht vanuit FHIR R4. De huidige SIMVerrichting bevat geen statusveld 🔴. Herkomst en eventuele afleiding vanuit de verrichtingsregistratie nader te bepalen.",
      "mapping" : [{
        "identity" : "ProcedureSanToNlCore",
        "map" : "Procedure.status",
        "comment" : "Beide 1..1, required FHIR EventStatus. Herkomst vanuit SIM nog te bepalen."
      }]
    },
    {
      "id" : "Procedure.statusReason",
      "path" : "Procedure.statusReason",
      "max" : "0"
    },
    {
      "id" : "Procedure.category",
      "path" : "Procedure.category",
      "short" : "Santeon zorgprofielklasse",
      "comment" : "Gebaseerd op de lokale Santeon VerrichtingCategorieCodelijst. Deze classificatie is niet één-op-één equivalent aan de Nictiz GBB VerrichtingCategorieCodelijst 🟠.",
      "mapping" : [{
        "identity" : "ProcedureSanToNlCore",
        "map" : "Procedure.category",
        "comment" : "Beide 0..1. Santeon lokale zorgprofielklassen; Nictiz preferred SNOMED ValueSet. Geen directe één-op-één-equivalentie."
      }]
    },
    {
      "id" : "Procedure.category.coding.system",
      "path" : "Procedure.category.coding.system",
      "mapping" : [{
        "identity" : "sim",
        "map" : "Verrichting;VerrichtingCategorieSysteem"
      }]
    },
    {
      "id" : "Procedure.category.coding.code",
      "path" : "Procedure.category.coding.code",
      "mapping" : [{
        "identity" : "sim",
        "map" : "Verrichting;VerrichtingCategorieCode"
      }]
    },
    {
      "id" : "Procedure.category.coding.display",
      "path" : "Procedure.category.coding.display",
      "mapping" : [{
        "identity" : "sim",
        "map" : "Verrichting;VerrichtingCategorieOmschrijving"
      }]
    },
    {
      "id" : "Procedure.code",
      "path" : "Procedure.code",
      "mapping" : [{
        "identity" : "ProcedureSanToNlCore",
        "map" : "Procedure.code",
        "comment" : "Santeon 0..1 versus nl-core 1..1. Nictiz preferred SNOMED ValueSet; Santeon NZa, DHD en/of SNOMED CT."
      }]
    },
    {
      "id" : "Procedure.code.coding",
      "path" : "Procedure.code.coding",
      "min" : 1,
      "mapping" : [{
        "identity" : "ProcedureSanToNlCore",
        "map" : "Procedure.code.coding",
        "comment" : "Santeon vereist minimaal een Coding wanneer code aanwezig is. Meerdere coderingen uitsluitend bij semantische equivalentie."
      }]
    },
    {
      "id" : "Procedure.code.coding.system",
      "path" : "Procedure.code.coding.system",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "Verrichting;VerrichtingTypeCodeNZaSysteem",
        "comment" : "NZa-codering."
      },
      {
        "identity" : "sim",
        "map" : "Verrichting;VerrichtingTypeCodeSnomedCTSyst",
        "comment" : "SNOMED CT-codering."
      },
      {
        "identity" : "sim",
        "map" : "Verrichting;VerrichtingTypeCodeDHDSysteem",
        "comment" : "DHD-codering."
      }]
    },
    {
      "id" : "Procedure.code.coding.code",
      "path" : "Procedure.code.coding.code",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "Verrichting;VerrichtingTypeCodeNZa",
        "comment" : "NZa-codering."
      },
      {
        "identity" : "sim",
        "map" : "Verrichting;VerrichtingTypeCodeSnomedCT",
        "comment" : "SNOMED CT-codering."
      },
      {
        "identity" : "sim",
        "map" : "Verrichting;VerrichtingTypeCodeDHD",
        "comment" : "DHD-codering."
      }]
    },
    {
      "id" : "Procedure.code.coding.display",
      "path" : "Procedure.code.coding.display",
      "mapping" : [{
        "identity" : "sim",
        "map" : "Verrichting;VerrichtingTypeOmschrijvingNZa",
        "comment" : "NZa-codering."
      },
      {
        "identity" : "sim",
        "map" : "Verrichting;VerrichtingTypeOmschrijvingSnomedCT",
        "comment" : "SNOMED CT-codering."
      },
      {
        "identity" : "sim",
        "map" : "Verrichting;VerrichtingTypeOmschrijvingDHD",
        "comment" : "DHD-codering."
      }]
    },
    {
      "id" : "Procedure.subject",
      "path" : "Procedure.subject",
      "type" : [{
        "code" : "Reference",
        "targetProfile" : ["https://ig.santeon.nl/sim-on-fhir/StructureDefinition/patient-san"]
      }],
      "mapping" : [{
        "identity" : "sim",
        "map" : "Verrichting;Identificatienummer",
        "comment" : "Reference naar gepseudonimiseerde PatientSan op basis van patiëntidentifier."
      },
      {
        "identity" : "ProcedureSanToNlCore",
        "map" : "Procedure.subject",
        "comment" : "Beide 1..1. Santeon Reference(PatientSan); nl-core ondersteunt onder andere nl-core-Patient."
      }]
    },
    {
      "id" : "Procedure.encounter",
      "path" : "Procedure.encounter",
      "type" : [{
        "code" : "Reference",
        "targetProfile" : ["https://ig.santeon.nl/sim-on-fhir/StructureDefinition/encounter-san"]
      }],
      "mapping" : [{
        "identity" : "sim",
        "map" : "Verrichting;ContactID",
        "comment" : "Reference naar Encounter op basis van ContactID."
      },
      {
        "identity" : "ProcedureSanToNlCore",
        "map" : "Procedure.encounter",
        "comment" : "Santeon Reference(EncounterSan). Doelprofiel en eventuele aanvullende constraints verschillen."
      }]
    },
    {
      "id" : "Procedure.performed[x]",
      "path" : "Procedure.performed[x]",
      "slicing" : {
        "discriminator" : [{
          "type" : "type",
          "path" : "$this"
        }],
        "ordered" : false,
        "rules" : "open"
      },
      "type" : [{
        "code" : "dateTime"
      },
      {
        "code" : "Period"
      }],
      "mapping" : [{
        "identity" : "ProcedureSanToNlCore",
        "map" : "Procedure.performed[x]",
        "comment" : "Santeon beperkt tot dateTime of Period; nl-core ondersteunt daarnaast andere FHIR R4-varianten."
      }]
    },
    {
      "id" : "Procedure.performed[x]:performedPeriod",
      "path" : "Procedure.performed[x]",
      "sliceName" : "performedPeriod",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Period"
      }]
    },
    {
      "id" : "Procedure.performed[x]:performedPeriod.start",
      "path" : "Procedure.performed[x].start",
      "mapping" : [{
        "identity" : "sim",
        "map" : "Verrichting;VerrichtingStartDatum",
        "comment" : "Verrichting met tijdsduur; dateshift."
      },
      {
        "identity" : "ProcedureSanToNlCore",
        "map" : "Procedure.performedPeriod.start",
        "comment" : "Startmoment van verrichting."
      }]
    },
    {
      "id" : "Procedure.performed[x]:performedPeriod.end",
      "path" : "Procedure.performed[x].end",
      "mapping" : [{
        "identity" : "ProcedureSanToNlCore",
        "map" : "Procedure.performedPeriod.end",
        "comment" : "Eindmoment; nog niet actief in huidige SIM-scope."
      }]
    },
    {
      "id" : "Procedure.performed[x]:performedDateTime",
      "path" : "Procedure.performed[x]",
      "sliceName" : "performedDateTime",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "sim",
        "map" : "Verrichting;VerrichtingStartDatum",
        "comment" : "Momentane verrichting; dateshift."
      },
      {
        "identity" : "ProcedureSanToNlCore",
        "map" : "Procedure.performedDateTime",
        "comment" : "Moment van verrichting; Nictiz ondersteunt deze representatie."
      }]
    },
    {
      "id" : "Procedure.recorder",
      "path" : "Procedure.recorder",
      "max" : "0"
    },
    {
      "id" : "Procedure.asserter",
      "path" : "Procedure.asserter",
      "max" : "0"
    },
    {
      "id" : "Procedure.location",
      "path" : "Procedure.location",
      "max" : "0"
    },
    {
      "id" : "Procedure.reasonCode",
      "path" : "Procedure.reasonCode",
      "max" : "0"
    },
    {
      "id" : "Procedure.reasonReference",
      "path" : "Procedure.reasonReference",
      "max" : "0"
    },
    {
      "id" : "Procedure.bodySite",
      "path" : "Procedure.bodySite",
      "max" : "0"
    },
    {
      "id" : "Procedure.outcome",
      "path" : "Procedure.outcome",
      "max" : "0"
    },
    {
      "id" : "Procedure.report",
      "path" : "Procedure.report",
      "max" : "0"
    },
    {
      "id" : "Procedure.complication",
      "path" : "Procedure.complication",
      "max" : "0"
    },
    {
      "id" : "Procedure.complicationDetail",
      "path" : "Procedure.complicationDetail",
      "max" : "0"
    },
    {
      "id" : "Procedure.followUp",
      "path" : "Procedure.followUp",
      "max" : "0"
    },
    {
      "id" : "Procedure.note",
      "path" : "Procedure.note",
      "max" : "0"
    },
    {
      "id" : "Procedure.focalDevice",
      "path" : "Procedure.focalDevice",
      "max" : "0"
    },
    {
      "id" : "Procedure.usedReference",
      "path" : "Procedure.usedReference",
      "max" : "0"
    },
    {
      "id" : "Procedure.usedCode",
      "path" : "Procedure.usedCode",
      "max" : "0"
    }]
  }
}

```
