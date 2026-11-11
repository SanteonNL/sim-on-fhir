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
  "date" : "2026-11-11T17:43:54+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Generiek Santeon FHIR R4-profiel voor ingevulde vragenlijsten.\n\nGebaseerd op het Santeon Informatiemodel (SIMVragenlijstAntwoord).\nHet SIM levert één record per antwoord; records met hetzelfde\nVragenlijstAntwoordID vormen samen één QuestionnaireResponse.\n\nDe-identificatie wordt uitgevoerd volgens afzonderlijke\ntransformatiespecificaties en is geen inherente\neigenschap van dit profiel.",
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
    "comment" : "Informatieve mapping van FHIR QuestionnaireResponse naar SIM CSV-bronvelden."
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
      "min" : 1
    },
    {
      "id" : "QuestionnaireResponse.identifier.use",
      "path" : "QuestionnaireResponse.identifier.use",
      "max" : "0"
    },
    {
      "id" : "QuestionnaireResponse.identifier.system",
      "path" : "QuestionnaireResponse.identifier.system",
      "mapping" : [{
        "identity" : "sim",
        "map" : "VragenlijstAntwoord;VragenlijstAntwoordID",
        "comment" : "Systeemdeel van de samengestelde identifier."
      }]
    },
    {
      "id" : "QuestionnaireResponse.identifier.value",
      "path" : "QuestionnaireResponse.identifier.value",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "VragenlijstAntwoord;VragenlijstAntwoordID",
        "comment" : "Waardedeel van de samengestelde identifier; hash."
      }]
    },
    {
      "id" : "QuestionnaireResponse.questionnaire",
      "path" : "QuestionnaireResponse.questionnaire",
      "mapping" : [{
        "identity" : "sim",
        "map" : "VragenlijstAntwoord;QuestionnaireResponseQuestionnaire",
        "comment" : "Canonical-verwijzing naar de Questionnaire."
      }]
    },
    {
      "id" : "QuestionnaireResponse.subject",
      "path" : "QuestionnaireResponse.subject",
      "min" : 1,
      "type" : [{
        "code" : "Reference",
        "targetProfile" : ["https://ig.santeon.nl/sim-on-fhir/StructureDefinition/patient-san"]
      }],
      "mapping" : [{
        "identity" : "sim",
        "map" : "VragenlijstAntwoord;Identificatienummer",
        "comment" : "Patient-reference op basis van patiëntidentifier."
      }]
    },
    {
      "id" : "QuestionnaireResponse.authored",
      "path" : "QuestionnaireResponse.authored",
      "mapping" : [{
        "identity" : "sim",
        "map" : "VragenlijstAntwoord;InvulDatumTijd"
      }]
    },
    {
      "id" : "QuestionnaireResponse.item.linkId",
      "path" : "QuestionnaireResponse.item.linkId",
      "mapping" : [{
        "identity" : "sim",
        "map" : "VragenlijstAntwoord;QuestionnaireResponseItemLinkId",
        "comment" : "Verwijzing naar het item in de Questionnaire."
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
      },
      "type" : [{
        "code" : "dateTime"
      },
      {
        "code" : "Coding"
      },
      {
        "code" : "Quantity"
      }]
    },
    {
      "id" : "QuestionnaireResponse.item.answer.value[x]:valueQuantity",
      "path" : "QuestionnaireResponse.item.answer.value[x]",
      "sliceName" : "valueQuantity",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Quantity"
      }]
    },
    {
      "id" : "QuestionnaireResponse.item.answer.value[x]:valueQuantity.value",
      "path" : "QuestionnaireResponse.item.answer.value[x].value",
      "mapping" : [{
        "identity" : "sim",
        "map" : "VragenlijstAntwoord;AntwoordWaarde"
      }]
    },
    {
      "id" : "QuestionnaireResponse.item.answer.value[x]:valueQuantity.comparator",
      "path" : "QuestionnaireResponse.item.answer.value[x].comparator",
      "mapping" : [{
        "identity" : "sim",
        "map" : "VragenlijstAntwoord;AntwoordWaardeOperator"
      }]
    },
    {
      "id" : "QuestionnaireResponse.item.answer.value[x]:valueQuantity.system",
      "path" : "QuestionnaireResponse.item.answer.value[x].system",
      "mapping" : [{
        "identity" : "sim",
        "map" : "VragenlijstAntwoord;AntwoordWaardeEenheidSysteem"
      }]
    },
    {
      "id" : "QuestionnaireResponse.item.answer.value[x]:valueQuantity.code",
      "path" : "QuestionnaireResponse.item.answer.value[x].code",
      "mapping" : [{
        "identity" : "sim",
        "map" : "VragenlijstAntwoord;AntwoordWaardeEenheid"
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
      }]
    },
    {
      "id" : "QuestionnaireResponse.item.answer.value[x]:valueCoding.system",
      "path" : "QuestionnaireResponse.item.answer.value[x].system",
      "mapping" : [{
        "identity" : "sim",
        "map" : "VragenlijstAntwoord;AntwoordCodeSysteem"
      }]
    },
    {
      "id" : "QuestionnaireResponse.item.answer.value[x]:valueCoding.code",
      "path" : "QuestionnaireResponse.item.answer.value[x].code",
      "mapping" : [{
        "identity" : "sim",
        "map" : "VragenlijstAntwoord;AntwoordCode"
      }]
    },
    {
      "id" : "QuestionnaireResponse.item.answer.value[x]:valueCoding.display",
      "path" : "QuestionnaireResponse.item.answer.value[x].display",
      "mapping" : [{
        "identity" : "sim",
        "map" : "VragenlijstAntwoord;AntwoordCodeOmschrijving"
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
        "identity" : "sim",
        "map" : "VragenlijstAntwoord;AntwoordDatumTijd"
      }]
    }]
  }
}

```
