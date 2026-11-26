# Encounter (SAN) - SIM on FHIR v0.1.0

## Resource Profile: Encounter (SAN) 

**Usages:**

* This Profile is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-encounter-san.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-encounter-san.csv), [Excel](../StructureDefinition-encounter-san.xlsx), [Schematron](../StructureDefinition-encounter-san.sch) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "encounter-san",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/encounter-san",
  "version" : "0.1.0",
  "name" : "EncounterSan",
  "title" : "Encounter (SAN)",
  "status" : "draft",
  "date" : "2026-11-26T10:18:33+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Santeon FHIR R4-profiel voor zorgcontacten, gebaseerd op het Santeon Informatiemodel (SIMContact) en afgeleid van FHIR R4 Encounter.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "EncounterSanFromSIMContact",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMContact",
    "name" : "SIM Logical Model SIMContact",
    "comment" : "Herkomst van de EncounterSan-elementen in Contact (SIMContact), met de conversietoelichting. Afgeleid van SIMContactToFHIR; wijzigingen alleen daar aanbrengen."
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
  "type" : "Encounter",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Encounter",
  "derivation" : "constraint",
  "differential" : {
    "element" : [{
      "id" : "Encounter",
      "path" : "Encounter"
    },
    {
      "id" : "Encounter.identifier",
      "path" : "Encounter.identifier",
      "mapping" : [{
        "identity" : "EncounterSanFromSIMContact",
        "map" : "SIMContact.ContactID",
        "comment" : "Direct: samengestelde identifier (system|value); het waardedeel wordt gehasht volgens de ETL-specificatie."
      }]
    },
    {
      "id" : "Encounter.class",
      "path" : "Encounter.class",
      "mapping" : [{
        "identity" : "EncounterSanFromSIMContact",
        "map" : "SIMContact.ContactType",
        "comment" : "Direct: SIM Coding wordt overgenomen als FHIR Coding; aansluiting van Santeon_ContactTypeCodelijst op de FHIR-binding nog te beoordelen."
      }]
    },
    {
      "id" : "Encounter.priority",
      "path" : "Encounter.priority",
      "mapping" : [{
        "identity" : "EncounterSanFromSIMContact",
        "map" : "SIMContact.ContactPrioriteit",
        "comment" : "Direct: SIM en FHIR gebruiken beide v3-ActPriority (example)."
      }]
    },
    {
      "id" : "Encounter.subject",
      "path" : "Encounter.subject",
      "mapping" : [{
        "identity" : "EncounterSanFromSIMContact",
        "map" : "SIMContact.PatientIdentificatienummer",
        "comment" : "Structuurconversie: Patient-reference op basis van identificatienummer."
      }]
    },
    {
      "id" : "Encounter.episodeOfCare",
      "path" : "Encounter.episodeOfCare",
      "mapping" : [{
        "identity" : "EncounterSanFromSIMContact",
        "map" : "SIMContact.SubtrajectNummer",
        "comment" : "Structuurconversie: EpisodeOfCare-reference op basis van SubtrajectNummer, indien het DBC-subtraject als EpisodeOfCare wordt gerepresenteerd."
      }]
    },
    {
      "id" : "Encounter.period.start",
      "path" : "Encounter.period.start",
      "mapping" : [{
        "identity" : "EncounterSanFromSIMContact",
        "map" : "SIMContact.BeginDatumTijd",
        "comment" : "Structuurconversie: SIM BeginDatumTijd en EindDatumTijd vormen samen de FHIR Period; dateshift volgens afzonderlijke transformatiespecificatie."
      }]
    },
    {
      "id" : "Encounter.period.end",
      "path" : "Encounter.period.end",
      "mapping" : [{
        "identity" : "EncounterSanFromSIMContact",
        "map" : "SIMContact.EindDatumTijd",
        "comment" : "Structuurconversie: SIM BeginDatumTijd en EindDatumTijd vormen samen de FHIR Period; dateshift volgens afzonderlijke transformatiespecificatie."
      }]
    },
    {
      "id" : "Encounter.hospitalization.dischargeDisposition",
      "path" : "Encounter.hospitalization.dischargeDisposition",
      "mapping" : [{
        "identity" : "EncounterSanFromSIMContact",
        "map" : "SIMContact.Bestemming",
        "comment" : "Direct: wanneer het gegeven de ontslagbestemming beschrijft."
      }]
    }]
  }
}

```
