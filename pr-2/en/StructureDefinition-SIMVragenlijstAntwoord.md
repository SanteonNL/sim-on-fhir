# Vragenlijstantwoord (model) - SIM on FHIR v0.1.0

## Logical Model: Vragenlijstantwoord (model) 

**Usages:**

* This Logical Model is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-SIMVragenlijstAntwoord.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-SIMVragenlijstAntwoord.csv), [Excel](../StructureDefinition-SIMVragenlijstAntwoord.xlsx) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "SIMVragenlijstAntwoord",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMVragenlijstAntwoord",
  "version" : "0.1.0",
  "name" : "SIMVragenlijstAntwoord",
  "title" : "Vragenlijstantwoord (model)",
  "status" : "draft",
  "date" : "2026-11-11T17:38:18+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Generiek logisch model voor een antwoord op een vraag uit een ingevulde vragenlijst binnen het Santeon Informatiemodel.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "SIMVragenlijstAntwoordFromSIMCSV",
    "uri" : "urn:santeon:sim:csv",
    "name" : "SIM CSV"
  },
  {
    "identity" : "SIMVragenlijstAntwoordToFHIR",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/questionnaireResponse-san",
    "name" : "SIMVragenlijstAntwoord naar FHIR"
  }],
  "kind" : "logical",
  "abstract" : false,
  "type" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMVragenlijstAntwoord",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Base",
  "derivation" : "specialization",
  "differential" : {
    "element" : [{
      "id" : "SIMVragenlijstAntwoord",
      "path" : "SIMVragenlijstAntwoord",
      "short" : "Vragenlijstantwoord (model)",
      "definition" : "Generiek logisch model voor een antwoord op een vraag uit een ingevulde vragenlijst binnen het Santeon Informatiemodel."
    },
    {
      "id" : "SIMVragenlijstAntwoord.VragenlijstAntwoordID",
      "path" : "SIMVragenlijstAntwoord.VragenlijstAntwoordID",
      "short" : "Uniek ID (key) binnen patiënt per ingevulde vragenlijst",
      "definition" : "Uniek ID (key) binnen patiënt per ingevulde vragenlijst",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMVragenlijstAntwoordFromSIMCSV",
        "map" : "VragenlijstAntwoord;VragenlijstAntwoordID"
      },
      {
        "identity" : "SIMVragenlijstAntwoordToFHIR",
        "map" : "QuestionnaireResponse.identifier"
      }]
    },
    {
      "id" : "SIMVragenlijstAntwoord.PatientIdentificatienummer",
      "path" : "SIMVragenlijstAntwoord.PatientIdentificatienummer",
      "short" : "Identificatienummer van de patiënt",
      "definition" : "Identificatienummer van de patiënt",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMVragenlijstAntwoordFromSIMCSV",
        "map" : "VragenlijstAntwoord;Identificatienummer"
      },
      {
        "identity" : "SIMVragenlijstAntwoordToFHIR",
        "map" : "QuestionnaireResponse.subject",
        "comment" : "Patient-reference op basis van identificatienummer; SIM benoemt daarnaast QuestionnaireResponse.source."
      }]
    },
    {
      "id" : "SIMVragenlijstAntwoord.InvulDatumTijd",
      "path" : "SIMVragenlijstAntwoord.InvulDatumTijd",
      "short" : "Datum en eventueel tijd waarop de antwoorden zijn verzameld",
      "definition" : "Datum en eventueel tijd waarop de antwoorden zijn verzameld",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "SIMVragenlijstAntwoordFromSIMCSV",
        "map" : "VragenlijstAntwoord;InvulDatumTijd"
      },
      {
        "identity" : "SIMVragenlijstAntwoordToFHIR",
        "map" : "QuestionnaireResponse.authored"
      }]
    },
    {
      "id" : "SIMVragenlijstAntwoord.Vragenlijst",
      "path" : "SIMVragenlijstAntwoord.Vragenlijst",
      "short" : "Code van de vragenlijst waartoe het antwoord behoort",
      "definition" : "Code van de vragenlijst waartoe het antwoord behoort",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Coding"
      }]
    },
    {
      "id" : "SIMVragenlijstAntwoord.Vragenlijst.system",
      "path" : "SIMVragenlijstAntwoord.Vragenlijst.system",
      "mapping" : [{
        "identity" : "SIMVragenlijstAntwoordFromSIMCSV",
        "map" : "VragenlijstAntwoord;QuestionnaireCodeCodingSystem"
      }]
    },
    {
      "id" : "SIMVragenlijstAntwoord.Vragenlijst.code",
      "path" : "SIMVragenlijstAntwoord.Vragenlijst.code",
      "mapping" : [{
        "identity" : "SIMVragenlijstAntwoordFromSIMCSV",
        "map" : "VragenlijstAntwoord;QuestionnaireCodeCodingCode"
      }]
    },
    {
      "id" : "SIMVragenlijstAntwoord.Vragenlijst.display",
      "path" : "SIMVragenlijstAntwoord.Vragenlijst.display",
      "mapping" : [{
        "identity" : "SIMVragenlijstAntwoordFromSIMCSV",
        "map" : "VragenlijstAntwoord;QuestionnaireCodeCodingDisplay"
      }]
    },
    {
      "id" : "SIMVragenlijstAntwoord.Vraag",
      "path" : "SIMVragenlijstAntwoord.Vraag",
      "short" : "Code van de vraag waarop het antwoord is gegeven",
      "definition" : "Code van de vraag waarop het antwoord is gegeven",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Coding"
      }]
    },
    {
      "id" : "SIMVragenlijstAntwoord.Vraag.system",
      "path" : "SIMVragenlijstAntwoord.Vraag.system",
      "mapping" : [{
        "identity" : "SIMVragenlijstAntwoordFromSIMCSV",
        "map" : "VragenlijstAntwoord;QuestionnaireItemCodeCodingSystem"
      }]
    },
    {
      "id" : "SIMVragenlijstAntwoord.Vraag.code",
      "path" : "SIMVragenlijstAntwoord.Vraag.code",
      "mapping" : [{
        "identity" : "SIMVragenlijstAntwoordFromSIMCSV",
        "map" : "VragenlijstAntwoord;QuestionnaireItemCodeCodingCode"
      }]
    },
    {
      "id" : "SIMVragenlijstAntwoord.Vraag.display",
      "path" : "SIMVragenlijstAntwoord.Vraag.display",
      "mapping" : [{
        "identity" : "SIMVragenlijstAntwoordFromSIMCSV",
        "map" : "VragenlijstAntwoord;QuestionnaireItemCodeCodingDisplay"
      }]
    },
    {
      "id" : "SIMVragenlijstAntwoord.AntwoordWaarde",
      "path" : "SIMVragenlijstAntwoord.AntwoordWaarde",
      "short" : "Numeriek antwoord op de vraag, met eventueel eenheid (UCUM) en vergelijkingsoperator",
      "definition" : "Numeriek antwoord op de vraag, met eventueel eenheid (UCUM) en vergelijkingsoperator",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Quantity"
      }],
      "mapping" : [{
        "identity" : "SIMVragenlijstAntwoordToFHIR",
        "map" : "QuestionnaireResponse.item.answer.valueQuantity"
      }]
    },
    {
      "id" : "SIMVragenlijstAntwoord.AntwoordWaarde.value",
      "path" : "SIMVragenlijstAntwoord.AntwoordWaarde.value",
      "mapping" : [{
        "identity" : "SIMVragenlijstAntwoordFromSIMCSV",
        "map" : "VragenlijstAntwoord;AntwoordWaarde"
      }]
    },
    {
      "id" : "SIMVragenlijstAntwoord.AntwoordWaarde.comparator",
      "path" : "SIMVragenlijstAntwoord.AntwoordWaarde.comparator",
      "mapping" : [{
        "identity" : "SIMVragenlijstAntwoordFromSIMCSV",
        "map" : "VragenlijstAntwoord;AntwoordWaardeOperator"
      }]
    },
    {
      "id" : "SIMVragenlijstAntwoord.AntwoordWaarde.system",
      "path" : "SIMVragenlijstAntwoord.AntwoordWaarde.system",
      "mapping" : [{
        "identity" : "SIMVragenlijstAntwoordFromSIMCSV",
        "map" : "VragenlijstAntwoord;AntwoordWaardeEenheidSysteem"
      }]
    },
    {
      "id" : "SIMVragenlijstAntwoord.AntwoordWaarde.code",
      "path" : "SIMVragenlijstAntwoord.AntwoordWaarde.code",
      "mapping" : [{
        "identity" : "SIMVragenlijstAntwoordFromSIMCSV",
        "map" : "VragenlijstAntwoord;AntwoordWaardeEenheid"
      }]
    },
    {
      "id" : "SIMVragenlijstAntwoord.AntwoordCode",
      "path" : "SIMVragenlijstAntwoord.AntwoordCode",
      "short" : "Gecodeerd antwoord op de vraag",
      "definition" : "Gecodeerd antwoord op de vraag",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Coding"
      }],
      "mapping" : [{
        "identity" : "SIMVragenlijstAntwoordToFHIR",
        "map" : "QuestionnaireResponse.item.answer.valueCoding"
      }]
    },
    {
      "id" : "SIMVragenlijstAntwoord.AntwoordCode.system",
      "path" : "SIMVragenlijstAntwoord.AntwoordCode.system",
      "mapping" : [{
        "identity" : "SIMVragenlijstAntwoordFromSIMCSV",
        "map" : "VragenlijstAntwoord;AntwoordCodeSysteem"
      }]
    },
    {
      "id" : "SIMVragenlijstAntwoord.AntwoordCode.code",
      "path" : "SIMVragenlijstAntwoord.AntwoordCode.code",
      "mapping" : [{
        "identity" : "SIMVragenlijstAntwoordFromSIMCSV",
        "map" : "VragenlijstAntwoord;AntwoordCode"
      }]
    },
    {
      "id" : "SIMVragenlijstAntwoord.AntwoordCode.display",
      "path" : "SIMVragenlijstAntwoord.AntwoordCode.display",
      "mapping" : [{
        "identity" : "SIMVragenlijstAntwoordFromSIMCSV",
        "map" : "VragenlijstAntwoord;AntwoordCodeOmschrijving"
      }]
    },
    {
      "id" : "SIMVragenlijstAntwoord.AntwoordDatumTijd",
      "path" : "SIMVragenlijstAntwoord.AntwoordDatumTijd",
      "short" : "Datum en eventueel tijd als antwoord op de vraag",
      "definition" : "Datum en eventueel tijd als antwoord op de vraag",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "SIMVragenlijstAntwoordFromSIMCSV",
        "map" : "VragenlijstAntwoord;AntwoordDatumTijd"
      },
      {
        "identity" : "SIMVragenlijstAntwoordToFHIR",
        "map" : "QuestionnaireResponse.item.answer.valueDateTime"
      }]
    }]
  }
}

```
