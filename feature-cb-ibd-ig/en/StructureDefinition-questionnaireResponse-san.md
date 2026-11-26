# QuestionnaireResponse (SAN) - SIM on FHIR v0.1.0

## Resource Profile: QuestionnaireResponse (SAN) 

**Usages:**

* This Profile is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-questionnaireResponse-san.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-questionnaireResponse-san.csv), [Excel](../StructureDefinition-questionnaireResponse-san.xlsx), [Schematron](../StructureDefinition-questionnaireResponse-san.sch) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "questionnaireResponse-san",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/questionnaireResponse-san",
  "version" : "0.1.0",
  "name" : "QuestionnaireResponseSan",
  "title" : "QuestionnaireResponse (SAN)",
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
  "description" : "Santeon FHIR R4-profiel voor ingevulde vragenlijsten, gebaseerd op het Santeon Informatiemodel (SIMVragenlijstAntwoord) en afgeleid van FHIR R4 QuestionnaireResponse.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "QuestionnaireResponseSanFromSIMVragenlijstAntwoord",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMVragenlijstAntwoord",
    "name" : "SIM Logical Model SIMVragenlijstAntwoord",
    "comment" : "Herkomst van de QuestionnaireResponseSan-elementen in Vragenlijstantwoord (SIMVragenlijstAntwoord), met de conversietoelichting. Afgeleid van SIMVragenlijstAntwoordToFHIR; wijzigingen alleen daar aanbrengen."
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
  "type" : "QuestionnaireResponse",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/QuestionnaireResponse",
  "derivation" : "constraint",
  "differential" : {
    "element" : [{
      "id" : "QuestionnaireResponse",
      "path" : "QuestionnaireResponse"
    },
    {
      "id" : "QuestionnaireResponse.identifier",
      "path" : "QuestionnaireResponse.identifier",
      "mapping" : [{
        "identity" : "QuestionnaireResponseSanFromSIMVragenlijstAntwoord",
        "map" : "SIMVragenlijstAntwoord.VragenlijstAntwoordID",
        "comment" : "Direct: samengestelde identifier (system|value); het waardedeel wordt gehasht volgens de ETL-specificatie."
      }]
    },
    {
      "id" : "QuestionnaireResponse.subject",
      "path" : "QuestionnaireResponse.subject",
      "mapping" : [{
        "identity" : "QuestionnaireResponseSanFromSIMVragenlijstAntwoord",
        "map" : "SIMVragenlijstAntwoord.PatientIdentificatienummer",
        "comment" : "Structuurconversie: Patient-reference op basis van identificatienummer; SIM benoemt daarnaast QuestionnaireResponse.source."
      }]
    },
    {
      "id" : "QuestionnaireResponse.authored",
      "path" : "QuestionnaireResponse.authored",
      "mapping" : [{
        "identity" : "QuestionnaireResponseSanFromSIMVragenlijstAntwoord",
        "map" : "SIMVragenlijstAntwoord.InvulDatumTijd",
        "comment" : "Direct: SIM dateTime wordt overgenomen als FHIR dateTime."
      }]
    },
    {
      "id" : "QuestionnaireResponse.item.answer.value[x]",
      "path" : "QuestionnaireResponse.item.answer.value[x]",
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
      "id" : "QuestionnaireResponse.item.answer.value[x]:valueQuantity",
      "path" : "QuestionnaireResponse.item.answer.value[x]",
      "sliceName" : "valueQuantity",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Quantity"
      }],
      "mapping" : [{
        "identity" : "QuestionnaireResponseSanFromSIMVragenlijstAntwoord",
        "map" : "SIMVragenlijstAntwoord.AntwoordWaarde",
        "comment" : "Structuurconversie: één SIM-record wordt één item met één answer; SIM Quantity wordt overgenomen als FHIR Quantity."
      }]
    },
    {
      "id" : "QuestionnaireResponse.item.answer.value[x]:valueCoding",
      "path" : "QuestionnaireResponse.item.answer.value[x]",
      "sliceName" : "valueCoding",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Coding"
      }],
      "mapping" : [{
        "identity" : "QuestionnaireResponseSanFromSIMVragenlijstAntwoord",
        "map" : "SIMVragenlijstAntwoord.AntwoordCode",
        "comment" : "Structuurconversie: één SIM-record wordt één item met één answer; SIM Coding wordt overgenomen als FHIR Coding."
      }]
    },
    {
      "id" : "QuestionnaireResponse.item.answer.value[x]:valueDateTime",
      "path" : "QuestionnaireResponse.item.answer.value[x]",
      "sliceName" : "valueDateTime",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "QuestionnaireResponseSanFromSIMVragenlijstAntwoord",
        "map" : "SIMVragenlijstAntwoord.AntwoordDatumTijd",
        "comment" : "Structuurconversie: één SIM-record wordt één item met één answer; SIM dateTime wordt overgenomen als FHIR dateTime."
      }]
    }]
  }
}

```
