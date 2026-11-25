# EpisodeOfCare (SAN) - SIM on FHIR v0.1.0

## Resource Profile: EpisodeOfCare (SAN) 

**Usages:**

* This Profile is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-episodeOfCare-san.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-episodeOfCare-san.csv), [Excel](../StructureDefinition-episodeOfCare-san.xlsx), [Schematron](../StructureDefinition-episodeOfCare-san.sch) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "episodeOfCare-san",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/episodeOfCare-san",
  "version" : "0.1.0",
  "name" : "EpisodeOfCareSan",
  "title" : "EpisodeOfCare (SAN)",
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
  "description" : "Santeon FHIR R4-profiel voor zorgepisodes, gebaseerd op het Santeon Informatiemodel (SIMDBC) en afgeleid van FHIR R4 EpisodeOfCare.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "EpisodeOfCareSanFromSIMDBC",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMDBC",
    "name" : "SIM Logical Model SIMDBC",
    "comment" : "Herkomst van de EpisodeOfCareSan-elementen in DBC/Zorgtraject (SIMDBC), met de conversietoelichting. Afgeleid van SIMDBCToFHIR; wijzigingen alleen daar aanbrengen."
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
  }],
  "kind" : "resource",
  "abstract" : false,
  "type" : "EpisodeOfCare",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/EpisodeOfCare",
  "derivation" : "constraint",
  "differential" : {
    "element" : [{
      "id" : "EpisodeOfCare",
      "path" : "EpisodeOfCare"
    },
    {
      "id" : "EpisodeOfCare.identifier",
      "path" : "EpisodeOfCare.identifier",
      "mapping" : [{
        "identity" : "EpisodeOfCareSanFromSIMDBC",
        "map" : "SIMDBC.SubtrajectNummer",
        "comment" : "Direct: indien één EpisodeOfCare één DBC-subtraject representeert; samengestelde identifier (system|value), waardedeel gehasht volgens de ETL-specificatie."
      }]
    },
    {
      "id" : "EpisodeOfCare.type",
      "path" : "EpisodeOfCare.type",
      "mapping" : [{
        "identity" : "EpisodeOfCareSanFromSIMDBC",
        "map" : "SIMDBC.ZorgType",
        "comment" : "Direct: mits het zorgtype de zorgepisode correct classificeert."
      }]
    },
    {
      "id" : "EpisodeOfCare.patient",
      "path" : "EpisodeOfCare.patient",
      "mapping" : [{
        "identity" : "EpisodeOfCareSanFromSIMDBC",
        "map" : "SIMDBC.PatientIdentificatienummer",
        "comment" : "Structuurconversie: Patient-reference op basis van identificatienummer."
      }]
    },
    {
      "id" : "EpisodeOfCare.period.start",
      "path" : "EpisodeOfCare.period.start",
      "mapping" : [{
        "identity" : "EpisodeOfCareSanFromSIMDBC",
        "map" : "SIMDBC.OpeningsDatum",
        "comment" : "Datatypeconversie: SIM date naar FHIR dateTime met behoud van datumprecisie (geen tijdstip toevoegen); alleen indien de DBC-openingsdatum overeenkomt met de start van de zorgepisode."
      }]
    },
    {
      "id" : "EpisodeOfCare.period.end",
      "path" : "EpisodeOfCare.period.end",
      "mapping" : [{
        "identity" : "EpisodeOfCareSanFromSIMDBC",
        "map" : "SIMDBC.SluitingsDatum",
        "comment" : "Datatypeconversie: SIM date naar FHIR dateTime met behoud van datumprecisie (geen tijdstip toevoegen); alleen indien de DBC-sluitingsdatum overeenkomt met het einde van de zorgepisode."
      }]
    }]
  }
}

```
