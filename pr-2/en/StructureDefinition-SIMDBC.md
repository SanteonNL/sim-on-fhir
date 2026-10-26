# DBC/Zorgtraject (model) - SIM on FHIR v0.1.0

## Logical Model: DBC/Zorgtraject (model) 

**Usages:**

* This Logical Model is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-SIMDBC.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-SIMDBC.csv), [Excel](../StructureDefinition-SIMDBC.xlsx) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "SIMDBC",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMDBC",
  "version" : "0.1.0",
  "name" : "SIMDBC",
  "title" : "DBC/Zorgtraject (model)",
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
  "description" : "Logisch model voor een DBC binnen het Santeon Informatiemodel.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "SIMDBCFromSIMCSV",
    "uri" : "SIM CSVs",
    "name" : "SIM CSVs"
  },
  {
    "identity" : "SIMDBCToFHIR",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/episodeOfCare-san",
    "name" : "SIM DBC naar FHIR"
  }],
  "kind" : "logical",
  "abstract" : false,
  "type" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMDBC",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Base",
  "derivation" : "specialization",
  "differential" : {
    "element" : [{
      "id" : "SIMDBC",
      "path" : "SIMDBC",
      "short" : "DBC/Zorgtraject (model)",
      "definition" : "Logisch model voor een DBC binnen het Santeon Informatiemodel."
    },
    {
      "id" : "SIMDBC.Patient",
      "path" : "SIMDBC.Patient",
      "short" : "Patiënt waarop de DBC betrekking heeft",
      "definition" : "Patiënt waarop de DBC betrekking heeft",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMPatient"
      }],
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "Identificatienummer"
      },
      {
        "identity" : "SIMDBCToFHIR",
        "map" : "EpisodeOfCare.patient"
      }]
    },
    {
      "id" : "SIMDBC.SubtrajectNummer",
      "path" : "SIMDBC.SubtrajectNummer",
      "short" : "Uniek identificatienummer van het DBC-subtraject",
      "definition" : "Uniek identificatienummer van het DBC-subtraject",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "SubtrajectNr"
      },
      {
        "identity" : "SIMDBCToFHIR",
        "map" : "EpisodeOfCare.identifier"
      }]
    },
    {
      "id" : "SIMDBC.ZorgtrajectNummer",
      "path" : "SIMDBC.ZorgtrajectNummer",
      "short" : "Identificatienummer van het zorgtraject",
      "definition" : "Identificatienummer van het zorgtraject",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "ZorgtrajectNr"
      }]
    },
    {
      "id" : "SIMDBC.Diagnose",
      "path" : "SIMDBC.Diagnose",
      "short" : "Diagnosecode en omschrijving",
      "definition" : "Diagnosecode en omschrijving",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "string"
      }]
    },
    {
      "id" : "SIMDBC.SpecialismeDiagnose",
      "path" : "SIMDBC.SpecialismeDiagnose",
      "short" : "Specialisme en diagnose behorend bij de DBC",
      "definition" : "Specialisme en diagnose behorend bij de DBC",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }],
      "binding" : {
        "strength" : "required",
        "valueSet" : "https://ig.santeon.nl/sim-on-fhir/ValueSet/Specialisme_DiagnoseCodelijstDummy"
      },
      "mapping" : [{
        "identity" : "SIMDBCToFHIR",
        "map" : "EpisodeOfCare.diagnosis.condition"
      }]
    },
    {
      "id" : "SIMDBC.SpecialismeDiagnose.coding.system",
      "path" : "SIMDBC.SpecialismeDiagnose.coding.system",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "SpecialismeDiagnoseSysteem"
      }]
    },
    {
      "id" : "SIMDBC.SpecialismeDiagnose.coding.code",
      "path" : "SIMDBC.SpecialismeDiagnose.coding.code",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "SpecialismeDiagnoseCode"
      }]
    },
    {
      "id" : "SIMDBC.SpecialismeDiagnose.coding.display",
      "path" : "SIMDBC.SpecialismeDiagnose.coding.display",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "SpecialismeDiagnoseOmschrijving"
      }]
    },
    {
      "id" : "SIMDBC.ZorgType",
      "path" : "SIMDBC.ZorgType",
      "short" : "Zorgtype van de DBC",
      "definition" : "Zorgtype van de DBC",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }],
      "mapping" : [{
        "identity" : "SIMDBCToFHIR",
        "map" : "EpisodeOfCare.type"
      }]
    },
    {
      "id" : "SIMDBC.ZorgType.coding.system",
      "path" : "SIMDBC.ZorgType.coding.system",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "ZorgTypeCodeSysteem"
      }]
    },
    {
      "id" : "SIMDBC.ZorgType.coding.code",
      "path" : "SIMDBC.ZorgType.coding.code",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "ZorgTypeCode"
      }]
    },
    {
      "id" : "SIMDBC.ZorgType.coding.display",
      "path" : "SIMDBC.ZorgType.coding.display",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "ZorgTypeOmschrijving"
      }]
    },
    {
      "id" : "SIMDBC.ZorgProduct",
      "path" : "SIMDBC.ZorgProduct",
      "short" : "Zorgproduct van de DBC",
      "definition" : "Zorgproduct van de DBC",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }]
    },
    {
      "id" : "SIMDBC.ZorgProduct.coding.system",
      "path" : "SIMDBC.ZorgProduct.coding.system",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "ZorgProductCodeSysteem"
      }]
    },
    {
      "id" : "SIMDBC.ZorgProduct.coding.code",
      "path" : "SIMDBC.ZorgProduct.coding.code",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "ZorgProductCode"
      }]
    },
    {
      "id" : "SIMDBC.ZorgProduct.coding.display",
      "path" : "SIMDBC.ZorgProduct.coding.display",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "ZorgProductOmschrijving"
      }]
    },
    {
      "id" : "SIMDBC.Gevuld",
      "path" : "SIMDBC.Gevuld",
      "short" : "Geeft aan of de DBC verrichtingen bevat",
      "definition" : "Geeft aan of de DBC verrichtingen bevat",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "boolean"
      }],
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "DBCgevuld"
      }]
    },
    {
      "id" : "SIMDBC.Geldig",
      "path" : "SIMDBC.Geldig",
      "short" : "Geeft aan of de DBC geldig is",
      "definition" : "Geeft aan of de DBC geldig is",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "boolean"
      }],
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "DBCgeldig"
      },
      {
        "identity" : "SIMDBCToFHIR",
        "map" : "EpisodeOfCare.status"
      }]
    },
    {
      "id" : "SIMDBC.OpeningsDatum",
      "path" : "SIMDBC.OpeningsDatum",
      "short" : "Openingsdatum van de DBC",
      "definition" : "Openingsdatum van de DBC",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "date"
      }],
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "DBCOpeningsDatum"
      },
      {
        "identity" : "SIMDBCToFHIR",
        "map" : "EpisodeOfCare.period.start"
      }]
    },
    {
      "id" : "SIMDBC.SluitingsDatum",
      "path" : "SIMDBC.SluitingsDatum",
      "short" : "Sluitingsdatum van de DBC",
      "definition" : "Sluitingsdatum van de DBC",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "date"
      }],
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "DBCSluitingsDatum"
      },
      {
        "identity" : "SIMDBCToFHIR",
        "map" : "EpisodeOfCare.period.end"
      }]
    },
    {
      "id" : "SIMDBC.UitvoerderSpecialisme",
      "path" : "SIMDBC.UitvoerderSpecialisme",
      "short" : "Specialisme van de uitvoerder",
      "definition" : "Specialisme van de uitvoerder",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Coding"
      }]
    },
    {
      "id" : "SIMDBC.UitvoerderSpecialisme.system",
      "path" : "SIMDBC.UitvoerderSpecialisme.system",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "Uitvoerder_SpecialismeSysteem"
      }]
    },
    {
      "id" : "SIMDBC.UitvoerderSpecialisme.code",
      "path" : "SIMDBC.UitvoerderSpecialisme.code",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "Uitvoerder_Specialisme"
      }]
    },
    {
      "id" : "SIMDBC.Verwijzer",
      "path" : "SIMDBC.Verwijzer",
      "short" : "Oorsprong van de verwijzing",
      "definition" : "Oorsprong van de verwijzing",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }]
    },
    {
      "id" : "SIMDBC.Verwijzer.coding.system",
      "path" : "SIMDBC.Verwijzer.coding.system",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "DBCverwijzerCodeSysteem"
      }]
    },
    {
      "id" : "SIMDBC.Verwijzer.coding.code",
      "path" : "SIMDBC.Verwijzer.coding.code",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "DBCverwijzerCode"
      }]
    },
    {
      "id" : "SIMDBC.Verwijzer.coding.display",
      "path" : "SIMDBC.Verwijzer.coding.display",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "DBCverwijzerCodeOmschrijving"
      }]
    }]
  }
}

```
