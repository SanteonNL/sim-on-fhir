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
  "date" : "2026-10-26T15:58:19+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "**De-identificatie:**\n- identifier → hashed\n- patient → hashed",
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
    "uri" : "http://hl7.org/fhir/workflow",
    "name" : "Santeon Informatie Model",
    "comment" : "Mapping van EpisodeOfCare elementen naar SIM"
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
      "id" : "EpisodeOfCare.meta",
      "path" : "EpisodeOfCare.meta",
      "max" : "0"
    },
    {
      "id" : "EpisodeOfCare.implicitRules",
      "path" : "EpisodeOfCare.implicitRules",
      "max" : "0"
    },
    {
      "id" : "EpisodeOfCare.language",
      "path" : "EpisodeOfCare.language",
      "max" : "0"
    },
    {
      "id" : "EpisodeOfCare.text",
      "path" : "EpisodeOfCare.text",
      "max" : "0"
    },
    {
      "id" : "EpisodeOfCare.contained",
      "path" : "EpisodeOfCare.contained",
      "max" : "0"
    },
    {
      "id" : "EpisodeOfCare.extension",
      "path" : "EpisodeOfCare.extension",
      "max" : "0"
    },
    {
      "id" : "EpisodeOfCare.modifierExtension",
      "path" : "EpisodeOfCare.modifierExtension",
      "max" : "0"
    },
    {
      "id" : "EpisodeOfCare.identifier",
      "path" : "EpisodeOfCare.identifier",
      "min" : 1,
      "max" : "1"
    },
    {
      "id" : "EpisodeOfCare.identifier.use",
      "path" : "EpisodeOfCare.identifier.use",
      "max" : "0"
    },
    {
      "id" : "EpisodeOfCare.identifier.value",
      "path" : "EpisodeOfCare.identifier.value",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "DBC;SubtrajectNr",
        "comment" : "hashed"
      }]
    },
    {
      "id" : "EpisodeOfCare.status",
      "path" : "EpisodeOfCare.status",
      "mapping" : [{
        "identity" : "sim",
        "map" : "DBC;DBCGeldig"
      }]
    },
    {
      "id" : "EpisodeOfCare.statusHistory",
      "path" : "EpisodeOfCare.statusHistory",
      "max" : "0"
    },
    {
      "id" : "EpisodeOfCare.type",
      "path" : "EpisodeOfCare.type",
      "min" : 1,
      "max" : "1"
    },
    {
      "id" : "EpisodeOfCare.type.coding",
      "path" : "EpisodeOfCare.type.coding",
      "min" : 1
    },
    {
      "id" : "EpisodeOfCare.type.coding.system",
      "path" : "EpisodeOfCare.type.coding.system",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "DBC;ZorgTypeCodeSysteem"
      }]
    },
    {
      "id" : "EpisodeOfCare.type.coding.code",
      "path" : "EpisodeOfCare.type.coding.code",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "DBC;ZorgTypeCode"
      }]
    },
    {
      "id" : "EpisodeOfCare.type.coding.display",
      "path" : "EpisodeOfCare.type.coding.display",
      "mapping" : [{
        "identity" : "sim",
        "map" : "DBC;ZorgTypeOmschrijving"
      }]
    },
    {
      "id" : "EpisodeOfCare.diagnosis",
      "path" : "EpisodeOfCare.diagnosis",
      "min" : 1
    },
    {
      "id" : "EpisodeOfCare.diagnosis.extension.extension",
      "path" : "EpisodeOfCare.diagnosis.extension.extension",
      "max" : "0"
    },
    {
      "id" : "EpisodeOfCare.diagnosis.extension.value[x]",
      "path" : "EpisodeOfCare.diagnosis.extension.value[x]",
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
      "id" : "EpisodeOfCare.diagnosis.extension.value[x]:valueCodeableConcept",
      "path" : "EpisodeOfCare.diagnosis.extension.value[x]",
      "sliceName" : "valueCodeableConcept",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }]
    },
    {
      "id" : "EpisodeOfCare.diagnosis.extension.value[x]:valueCodeableConcept.coding.system",
      "path" : "EpisodeOfCare.diagnosis.extension.value[x].coding.system",
      "mapping" : [{
        "identity" : "sim",
        "map" : "DBC;SpecialismeDiagnoseCodeSysteem"
      }]
    },
    {
      "id" : "EpisodeOfCare.diagnosis.extension.value[x]:valueCodeableConcept.coding.code",
      "path" : "EpisodeOfCare.diagnosis.extension.value[x].coding.code",
      "mapping" : [{
        "identity" : "sim",
        "map" : "DBC;SpecialismeDiagnoseCode"
      }]
    },
    {
      "id" : "EpisodeOfCare.diagnosis.extension.value[x]:valueCodeableConcept.coding.display",
      "path" : "EpisodeOfCare.diagnosis.extension.value[x].coding.display",
      "mapping" : [{
        "identity" : "sim",
        "map" : "DBC;SpecialismeDiagnoseOmschrijving"
      }]
    },
    {
      "id" : "EpisodeOfCare.diagnosis.modifierExtension",
      "path" : "EpisodeOfCare.diagnosis.modifierExtension",
      "max" : "0"
    },
    {
      "id" : "EpisodeOfCare.diagnosis.condition",
      "path" : "EpisodeOfCare.diagnosis.condition",
      "type" : [{
        "code" : "Reference",
        "targetProfile" : ["https://ig.santeon.nl/sim-on-fhir/StructureDefinition/condition-san"]
      }],
      "mapping" : [{
        "identity" : "sim",
        "map" : "DBC;SpecialismeDiagnoseCode",
        "comment" : "Reference naar ConditionSan; de specialismediagnose wordt gerepresenteerd in ConditionSan.code"
      }]
    },
    {
      "id" : "EpisodeOfCare.patient",
      "path" : "EpisodeOfCare.patient",
      "type" : [{
        "code" : "Reference",
        "targetProfile" : ["https://ig.santeon.nl/sim-on-fhir/StructureDefinition/patient-san"]
      }],
      "mapping" : [{
        "identity" : "sim",
        "map" : "DBC;Identificatienummer",
        "comment" : "hashed"
      }]
    },
    {
      "id" : "EpisodeOfCare.managingOrganization",
      "path" : "EpisodeOfCare.managingOrganization",
      "max" : "0"
    },
    {
      "id" : "EpisodeOfCare.period",
      "path" : "EpisodeOfCare.period",
      "min" : 1
    },
    {
      "id" : "EpisodeOfCare.period.start",
      "path" : "EpisodeOfCare.period.start",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "DBC;DBCOpeningsDatum"
      }]
    },
    {
      "id" : "EpisodeOfCare.referralRequest",
      "path" : "EpisodeOfCare.referralRequest",
      "max" : "0"
    },
    {
      "id" : "EpisodeOfCare.careManager",
      "path" : "EpisodeOfCare.careManager",
      "max" : "0"
    },
    {
      "id" : "EpisodeOfCare.team",
      "path" : "EpisodeOfCare.team",
      "max" : "0"
    },
    {
      "id" : "EpisodeOfCare.account",
      "path" : "EpisodeOfCare.account",
      "max" : "0"
    }]
  }
}

```
