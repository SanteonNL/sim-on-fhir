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
  "extension" : [{
    "url" : "http://hl7.org/fhir/StructureDefinition/structuredefinition-imposeProfile",
    "valueCanonical" : "http://hl7.eu/fhir/base/StructureDefinition/procedure-eu-core"
  }],
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/procedure-san",
  "version" : "0.1.0",
  "name" : "ProcedureSan",
  "title" : "Procedure (SAN)",
  "status" : "draft",
  "date" : "2026-11-26T10:19:07+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Santeon FHIR R4-profiel voor verrichtingen, gebaseerd op het Santeon Informatiemodel (SIMVerrichting) en afgeleid van Nictiz nl-core-Procedure.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "ProcedureSanFromSIMVerrichting",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMVerrichting",
    "name" : "SIM Logical Model SIMVerrichting",
    "comment" : "Herkomst van de ProcedureSan-elementen in Verrichting (SIMVerrichting), met de conversietoelichting. Afgeleid van SIMVerrichtingToFHIR; wijzigingen alleen daar aanbrengen."
  },
  {
    "identity" : "ProcedureSanToNlCore",
    "uri" : "http://ig.nictiz.nl/gbb/StructureDefinition/nl-core-Procedure",
    "name" : "Nictiz nl-core-Procedure",
    "comment" : "Technische delta-analyse tussen ProcedureSan en Nictiz nl-core-Procedure (referentieversie 1.0.0-alpha.2). Legt per FHIR-element binnen de IBD-scope de uitkomst van de vergelijking vast. Equivalent betekent dat het element is beoordeeld en er geen relevant technisch verschil is vastgesteld. 🟡 Verschil vastgesteld of nader te beoordelen."
  },
  {
    "identity" : "requirements-Procedure",
    "uri" : "http://ig.nictiz.nl/gbb/Requirements/Procedure",
    "name" : "Information requirements for Procedure"
  },
  {
    "identity" : "lm-Procedure",
    "uri" : "http://ig.nictiz.nl/gbb/StructureDefinition/LogicalModelProcedure",
    "name" : "Logical model for Procedure"
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
  "baseDefinition" : "http://ig.nictiz.nl/gbb/StructureDefinition/nl-core-Procedure",
  "derivation" : "constraint",
  "differential" : {
    "element" : [{
      "id" : "Procedure",
      "path" : "Procedure"
    },
    {
      "id" : "Procedure.identifier",
      "path" : "Procedure.identifier",
      "mapping" : [{
        "identity" : "ProcedureSanFromSIMVerrichting",
        "map" : "SIMVerrichting.VerrichtingID",
        "comment" : "Direct: samengestelde identifier (system|value); het waardedeel wordt gehasht volgens de ETL-specificatie."
      },
      {
        "identity" : "ProcedureSanToNlCore",
        "map" : "Procedure.identifier",
        "comment" : "🟡 nader te beoordelen"
      }]
    },
    {
      "id" : "Procedure.category",
      "path" : "Procedure.category",
      "mapping" : [{
        "identity" : "ProcedureSanFromSIMVerrichting",
        "map" : "SIMVerrichting.VerrichtingCategorie",
        "comment" : "Direct: SIM CodeableConcept wordt overgenomen als FHIR CodeableConcept."
      },
      {
        "identity" : "ProcedureSanToNlCore",
        "map" : "Procedure.category",
        "comment" : "🟡 nader te beoordelen"
      }]
    },
    {
      "id" : "Procedure.code",
      "path" : "Procedure.code",
      "mapping" : [{
        "identity" : "ProcedureSanFromSIMVerrichting",
        "map" : "SIMVerrichting.VerrichtingType",
        "comment" : "Direct: NZa-, SNOMED CT- en DHD-coderingen worden alleen gecombineerd wanneer zij hetzelfde concept vertegenwoordigen. Verschil in cardinaliteit: VerrichtingType is in het SIM 0..1, maar ProcedureSan erft Procedure.code 1..1 van nl-core-Procedure; een verrichting zonder VerrichtingType kan niet als ProcedureSan worden aangeleverd. De IBD-aanlevering selecteert al op code, zodat daar geen verrichtingen wegvallen."
      },
      {
        "identity" : "ProcedureSanToNlCore",
        "map" : "Procedure.code",
        "comment" : "🟡 nader te beoordelen"
      }]
    },
    {
      "id" : "Procedure.subject",
      "path" : "Procedure.subject",
      "mapping" : [{
        "identity" : "ProcedureSanFromSIMVerrichting",
        "map" : "SIMVerrichting.PatientIdentificatienummer",
        "comment" : "Structuurconversie: Patient-reference op basis van identificatienummer."
      },
      {
        "identity" : "ProcedureSanToNlCore",
        "map" : "Procedure.subject",
        "comment" : "🟡 nader te beoordelen"
      }]
    },
    {
      "id" : "Procedure.encounter",
      "path" : "Procedure.encounter",
      "mapping" : [{
        "identity" : "ProcedureSanFromSIMVerrichting",
        "map" : "SIMVerrichting.ContactID",
        "comment" : "Structuurconversie: Encounter-reference op basis van ContactID."
      },
      {
        "identity" : "ProcedureSanToNlCore",
        "map" : "Procedure.encounter",
        "comment" : "🟡 nader te beoordelen"
      }]
    },
    {
      "id" : "Procedure.performed[x]:performedPeriod",
      "path" : "Procedure.performed[x]",
      "sliceName" : "performedPeriod",
      "type" : [{
        "code" : "Period"
      }]
    },
    {
      "id" : "Procedure.performed[x]:performedPeriod.start",
      "path" : "Procedure.performed[x].start",
      "mapping" : [{
        "identity" : "ProcedureSanFromSIMVerrichting",
        "map" : "SIMVerrichting.VerrichtingStartDatum",
        "comment" : "Structuurconversie: bij een verrichting met tijdsduur performedPeriod.start, anders performedDateTime; dateshift volgens afzonderlijke transformatiespecificatie."
      },
      {
        "identity" : "ProcedureSanToNlCore",
        "map" : "Procedure.performed[x]:performedPeriod.start",
        "comment" : "🟡 nader te beoordelen"
      }]
    }]
  }
}

```
