# Observation (SAN) - SIM on FHIR v0.1.0

## Resource Profile: Observation (SAN) 

**Usages:**

* This Profile is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-observation-san.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-observation-san.csv), [Excel](../StructureDefinition-observation-san.xlsx), [Schematron](../StructureDefinition-observation-san.sch) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "observation-san",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/observation-san",
  "version" : "0.1.0",
  "name" : "ObservationSan",
  "title" : "Observation (SAN)",
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
  "description" : "Generiek Santeon FHIR R4-profiel voor algemene metingen.\n\nGebaseerd op het Santeon Informatiemodel (SIMAlgemeneMeting).\n\nDe-identificatie wordt uitgevoerd volgens afzonderlijke\ntransformatiespecificaties en is geen inherente\neigenschap van dit profiel.",
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
    "comment" : "Informatieve mapping van FHIR Observation naar SIM CSV-bronvelden."
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
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Observation",
  "derivation" : "constraint",
  "differential" : {
    "element" : [{
      "id" : "Observation",
      "path" : "Observation"
    },
    {
      "id" : "Observation.identifier",
      "path" : "Observation.identifier",
      "min" : 1,
      "max" : "1"
    },
    {
      "id" : "Observation.identifier.use",
      "path" : "Observation.identifier.use",
      "max" : "0"
    },
    {
      "id" : "Observation.identifier.system",
      "path" : "Observation.identifier.system",
      "max" : "0",
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;MetingID",
        "comment" : "Systeemdeel van de samengestelde identifier."
      }]
    },
    {
      "id" : "Observation.identifier.value",
      "path" : "Observation.identifier.value",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;MetingID",
        "comment" : "Waardedeel van de samengestelde identifier; hash."
      }]
    },
    {
      "id" : "Observation.code",
      "path" : "Observation.code",
      "binding" : {
        "strength" : "required",
        "valueSet" : "https://ig.santeon.nl/sim-on-fhir/ValueSet/Santeon-MetingNaamCodelijst"
      }
    },
    {
      "id" : "Observation.code.coding",
      "path" : "Observation.code.coding",
      "min" : 1
    },
    {
      "id" : "Observation.code.coding.system",
      "path" : "Observation.code.coding.system",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;MetingNaamCodeSysteem"
      }]
    },
    {
      "id" : "Observation.code.coding.code",
      "path" : "Observation.code.coding.code",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;MetingNaamCode"
      }]
    },
    {
      "id" : "Observation.code.coding.display",
      "path" : "Observation.code.coding.display",
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;MetingNaamOmschrijving"
      }]
    },
    {
      "id" : "Observation.subject",
      "path" : "Observation.subject",
      "min" : 1,
      "type" : [{
        "code" : "Reference",
        "targetProfile" : ["https://ig.santeon.nl/sim-on-fhir/StructureDefinition/patient-san"]
      }],
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;Identificatienummer",
        "comment" : "Patient-reference op basis van patiëntidentifier."
      }]
    },
    {
      "id" : "Observation.effective[x]",
      "path" : "Observation.effective[x]",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;MetingDatumTijd"
      }]
    },
    {
      "id" : "Observation.value[x]",
      "path" : "Observation.value[x]",
      "slicing" : {
        "discriminator" : [{
          "type" : "type",
          "path" : "$this"
        }],
        "ordered" : false,
        "rules" : "open"
      },
      "type" : [{
        "code" : "Quantity"
      },
      {
        "code" : "CodeableConcept"
      },
      {
        "code" : "Range"
      },
      {
        "code" : "dateTime"
      }]
    },
    {
      "id" : "Observation.value[x]:valueQuantity",
      "path" : "Observation.value[x]",
      "sliceName" : "valueQuantity",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Quantity"
      }]
    },
    {
      "id" : "Observation.value[x]:valueQuantity.value",
      "path" : "Observation.value[x].value",
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;UitslagWaarde"
      }]
    },
    {
      "id" : "Observation.value[x]:valueQuantity.comparator",
      "path" : "Observation.value[x].comparator",
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;UitslagWaardeOperator"
      }]
    },
    {
      "id" : "Observation.value[x]:valueQuantity.system",
      "path" : "Observation.value[x].system",
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;UitslagWaardeEenheidSysteem"
      }]
    },
    {
      "id" : "Observation.value[x]:valueQuantity.code",
      "path" : "Observation.value[x].code",
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;UitslagWaardeEenheid"
      }]
    },
    {
      "id" : "Observation.value[x]:valueRange",
      "path" : "Observation.value[x]",
      "sliceName" : "valueRange",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Range"
      }]
    },
    {
      "id" : "Observation.value[x]:valueRange.low.value",
      "path" : "Observation.value[x].low.value",
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;ObservationValueRangeLow"
      }]
    },
    {
      "id" : "Observation.value[x]:valueRange.low.unit",
      "path" : "Observation.value[x].low.unit",
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;ObservationValueRangeLowUnit"
      }]
    },
    {
      "id" : "Observation.value[x]:valueRange.low.system",
      "path" : "Observation.value[x].low.system",
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;ObservationValueRangeLowSystem"
      }]
    },
    {
      "id" : "Observation.value[x]:valueRange.low.code",
      "path" : "Observation.value[x].low.code",
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;ObservationValueRangeLowCode"
      }]
    },
    {
      "id" : "Observation.value[x]:valueRange.high.value",
      "path" : "Observation.value[x].high.value",
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;ObservationValueRangeHigh"
      }]
    },
    {
      "id" : "Observation.value[x]:valueRange.high.unit",
      "path" : "Observation.value[x].high.unit",
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;ObservationValueRangeHighUnit"
      }]
    },
    {
      "id" : "Observation.value[x]:valueRange.high.system",
      "path" : "Observation.value[x].high.system",
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;ObservationValueRangeHighSystem"
      }]
    },
    {
      "id" : "Observation.value[x]:valueRange.high.code",
      "path" : "Observation.value[x].high.code",
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;ObservationValueRangeHighCode"
      }]
    },
    {
      "id" : "Observation.value[x]:valueCodeableConcept",
      "path" : "Observation.value[x]",
      "sliceName" : "valueCodeableConcept",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }]
    },
    {
      "id" : "Observation.value[x]:valueCodeableConcept.coding.system",
      "path" : "Observation.value[x].coding.system",
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;UitslagCodeSysteem"
      }]
    },
    {
      "id" : "Observation.value[x]:valueCodeableConcept.coding.code",
      "path" : "Observation.value[x].coding.code",
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;UitslagCode"
      }]
    },
    {
      "id" : "Observation.value[x]:valueCodeableConcept.coding.display",
      "path" : "Observation.value[x].coding.display",
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;UitslagCodeOmschrijving"
      }]
    },
    {
      "id" : "Observation.value[x]:valueDateTime",
      "path" : "Observation.value[x]",
      "sliceName" : "valueDateTime",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;UitslagDatumTijd"
      }]
    },
    {
      "id" : "Observation.method.coding.system",
      "path" : "Observation.method.coding.system",
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;MeetmethodeCodeSysteem"
      }]
    },
    {
      "id" : "Observation.method.coding.code",
      "path" : "Observation.method.coding.code",
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;MeetmethodeCode"
      }]
    },
    {
      "id" : "Observation.method.coding.display",
      "path" : "Observation.method.coding.display",
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;MeetmethodeOmschrijving"
      }]
    }]
  }
}

```
