# Observation TabakGebruik (SAN) - SIM on FHIR v0.1.0

## Resource Profile: Observation TabakGebruik (SAN) 

**Usages:**

* This Profile is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-observation-san-tabakgebruik.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-observation-san-tabakgebruik.csv), [Excel](../StructureDefinition-observation-san-tabakgebruik.xlsx), [Schematron](../StructureDefinition-observation-san-tabakgebruik.sch) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "observation-san-tabakgebruik",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/observation-san-tabakgebruik",
  "version" : "0.1.0",
  "name" : "ObservationSanTabakGebruik",
  "title" : "Observation TabakGebruik (SAN)",
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
  "description" : "Santeon FHIR R4-profiel voor de vastgelegde rookstatus van een patiënt.\n\nAfgeleid van ObservationSan en gebaseerd op het\nSanteon Informatiemodel (SIMTabakGebruik).\n\nDe-identificatie wordt uitgevoerd volgens afzonderlijke\ntransformatiespecificaties en is geen inherente\neigenschap van dit profiel.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "sim-tabakgebruik",
    "uri" : "urn:santeon:sim:csv",
    "name" : "Santeon Informatiemodel (SIM CSV) - TabakGebruik",
    "comment" : "Informatieve mapping van FHIR Observation naar SIM TabakGebruik-bronvelden."
  },
  {
    "identity" : "workflow",
    "uri" : "http://hl7.org/fhir/workflow",
    "name" : "Workflow Pattern"
  },
  {
    "identity" : "sct-concept",
    "uri" : "http://snomed.info/conceptdomain",
    "name" : "SNOMED CT Concept Domain Binding"
  },
  {
    "identity" : "v2",
    "uri" : "http://hl7.org/v2",
    "name" : "HL7 v2 Mapping"
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
    "identity" : "sct-attr",
    "uri" : "http://snomed.org/attributebinding",
    "name" : "SNOMED CT Attribute Binding"
  }],
  "kind" : "resource",
  "abstract" : false,
  "type" : "Observation",
  "baseDefinition" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/observation-san",
  "derivation" : "constraint",
  "differential" : {
    "element" : [{
      "id" : "Observation",
      "path" : "Observation"
    },
    {
      "id" : "Observation.identifier.system",
      "path" : "Observation.identifier.system",
      "mapping" : [{
        "identity" : "sim-tabakgebruik",
        "map" : "TabakGebruik;MetingID",
        "comment" : "Systeemdeel van de samengestelde identifier."
      }]
    },
    {
      "id" : "Observation.identifier.value",
      "path" : "Observation.identifier.value",
      "mapping" : [{
        "identity" : "sim-tabakgebruik",
        "map" : "TabakGebruik;MetingID",
        "comment" : "Waardedeel van de samengestelde identifier; hash."
      }]
    },
    {
      "id" : "Observation.code",
      "path" : "Observation.code",
      "patternCodeableConcept" : {
        "coding" : [{
          "system" : "http://snomed.info/sct",
          "code" : "365980008"
        }]
      }
    },
    {
      "id" : "Observation.subject",
      "path" : "Observation.subject",
      "mapping" : [{
        "identity" : "sim-tabakgebruik",
        "map" : "TabakGebruik;Identificatienummer",
        "comment" : "Patient-reference op basis van patiëntidentifier."
      }]
    },
    {
      "id" : "Observation.effective[x]",
      "path" : "Observation.effective[x]",
      "mapping" : [{
        "identity" : "sim-tabakgebruik",
        "map" : "TabakGebruik;TabakGebruikStatusDatumVastleggen",
        "comment" : "Datum uit SIM; datumprecisie behouden."
      }]
    },
    {
      "id" : "Observation.value[x]",
      "path" : "Observation.value[x]",
      "type" : [{
        "code" : "CodeableConcept"
      }],
      "binding" : {
        "strength" : "extensible",
        "valueSet" : "https://ig.santeon.nl/sim-on-fhir/ValueSet/TabakGebruikStatusCodelijst"
      }
    },
    {
      "id" : "Observation.value[x].coding.system",
      "path" : "Observation.value[x].coding.system",
      "mapping" : [{
        "identity" : "sim-tabakgebruik",
        "map" : "TabakGebruik;TabakGebruikStatusSysteem"
      }]
    },
    {
      "id" : "Observation.value[x].coding.code",
      "path" : "Observation.value[x].coding.code",
      "mapping" : [{
        "identity" : "sim-tabakgebruik",
        "map" : "TabakGebruik;TabakGebruikStatus"
      }]
    },
    {
      "id" : "Observation.value[x].coding.display",
      "path" : "Observation.value[x].coding.display",
      "mapping" : [{
        "identity" : "sim-tabakgebruik",
        "map" : "TabakGebruik;TabakGebruikStatusOmschrijving"
      }]
    },
    {
      "id" : "Observation.method",
      "path" : "Observation.method",
      "max" : "0"
    }]
  }
}

```
