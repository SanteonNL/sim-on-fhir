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
  "date" : "2026-10-29T22:11:52+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Generiek logisch model voor een DBC binnen het Santeon Informatiemodel.",
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
    "uri" : "urn:santeon:sim:csv",
    "name" : "SIM CSV"
  },
  {
    "identity" : "SIMDBCToFHIR",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/episodeOfCare-san",
    "name" : "SIMDBC naar FHIR"
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
      "definition" : "Generiek logisch model voor een DBC binnen het Santeon Informatiemodel."
    },
    {
      "id" : "SIMDBC.PatientIdentificatienummer",
      "path" : "SIMDBC.PatientIdentificatienummer",
      "short" : "Identificatienummer van de patiënt",
      "definition" : "Identificatienummer van de patiënt",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "DBC;Identificatienummer"
      },
      {
        "identity" : "SIMDBCToFHIR",
        "map" : "EpisodeOfCare.patient",
        "comment" : "Patient-reference op basis van identificatienummer."
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
        "map" : "DBC;SubtrajectNr"
      },
      {
        "identity" : "SIMDBCToFHIR",
        "map" : "EpisodeOfCare.identifier",
        "comment" : "Indien één EpisodeOfCare één DBC-subtraject representeert."
      }]
    },
    {
      "id" : "SIMDBC.Diagnose",
      "path" : "SIMDBC.Diagnose",
      "short" : "Diagnose behorend bij de DBC",
      "definition" : "Diagnose behorend bij de DBC",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }]
    },
    {
      "id" : "SIMDBC.Diagnose.coding.code",
      "path" : "SIMDBC.Diagnose.coding.code",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "DBC;DiagnoseCode"
      }]
    },
    {
      "id" : "SIMDBC.Diagnose.coding.display",
      "path" : "SIMDBC.Diagnose.coding.display",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "DBC;DiagnoseOmschrijving"
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
      }]
    },
    {
      "id" : "SIMDBC.SpecialismeDiagnose.coding.system",
      "path" : "SIMDBC.SpecialismeDiagnose.coding.system",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "DBC;SpecialismeDiagnoseSysteem"
      }]
    },
    {
      "id" : "SIMDBC.SpecialismeDiagnose.coding.code",
      "path" : "SIMDBC.SpecialismeDiagnose.coding.code",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "DBC;SpecialismeDiagnoseCode"
      }]
    },
    {
      "id" : "SIMDBC.SpecialismeDiagnose.coding.display",
      "path" : "SIMDBC.SpecialismeDiagnose.coding.display",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "DBC;SpecialismeDiagnoseOmschrijving"
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
        "map" : "EpisodeOfCare.type",
        "comment" : "Mits het zorgtype de zorgepisode correct classificeert."
      }]
    },
    {
      "id" : "SIMDBC.ZorgType.coding.system",
      "path" : "SIMDBC.ZorgType.coding.system",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "DBC;ZorgTypeCodeSysteem"
      }]
    },
    {
      "id" : "SIMDBC.ZorgType.coding.code",
      "path" : "SIMDBC.ZorgType.coding.code",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "DBC;ZorgTypeCode"
      }]
    },
    {
      "id" : "SIMDBC.ZorgType.coding.display",
      "path" : "SIMDBC.ZorgType.coding.display",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "DBC;ZorgTypeOmschrijving"
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
        "map" : "DBC;ZorgProductCodeSysteem"
      }]
    },
    {
      "id" : "SIMDBC.ZorgProduct.coding.code",
      "path" : "SIMDBC.ZorgProduct.coding.code",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "DBC;ZorgProductCode"
      }]
    },
    {
      "id" : "SIMDBC.ZorgProduct.coding.display",
      "path" : "SIMDBC.ZorgProduct.coding.display",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "DBC;ZorgProductOmschrijving"
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
        "map" : "DBC;DBCgevuld"
      }]
    },
    {
      "id" : "SIMDBC.Geldig",
      "path" : "SIMDBC.Geldig",
      "short" : "Geeft aan of de DBC administratief geldig is",
      "definition" : "Geeft aan of de DBC administratief geldig is",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "boolean"
      }],
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "DBC;DBCgeldig"
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
        "map" : "DBC;DBCOpeningsDatum"
      },
      {
        "identity" : "SIMDBCToFHIR",
        "map" : "EpisodeOfCare.period.start",
        "comment" : "Alleen indien de DBC-openingsdatum overeenkomt met de start van de zorgepisode."
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
        "map" : "DBC;DBCSluitingsDatum"
      },
      {
        "identity" : "SIMDBCToFHIR",
        "map" : "EpisodeOfCare.period.end",
        "comment" : "Alleen indien de DBC-sluitingsdatum overeenkomt met het einde van de zorgepisode."
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
        "map" : "DBC;Uitvoerder_SpecialismeSysteem"
      }]
    },
    {
      "id" : "SIMDBC.UitvoerderSpecialisme.code",
      "path" : "SIMDBC.UitvoerderSpecialisme.code",
      "mapping" : [{
        "identity" : "SIMDBCFromSIMCSV",
        "map" : "DBC;Uitvoerder_Specialisme"
      }]
    }]
  }
}

```
