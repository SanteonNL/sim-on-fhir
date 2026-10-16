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
  "date" : "2026-10-16T14:27:57+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "**De-identificatie:**\n- identifier → hashed\n- subject → hashed\n- effectiveDateTime → dateshift",
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
    "comment" : "Mapping van Observation elementen naar SIM"
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
      "id" : "Observation.meta",
      "path" : "Observation.meta",
      "max" : "0"
    },
    {
      "id" : "Observation.implicitRules",
      "path" : "Observation.implicitRules",
      "max" : "0"
    },
    {
      "id" : "Observation.language",
      "path" : "Observation.language",
      "max" : "0"
    },
    {
      "id" : "Observation.text",
      "path" : "Observation.text",
      "max" : "0"
    },
    {
      "id" : "Observation.contained",
      "path" : "Observation.contained",
      "max" : "0"
    },
    {
      "id" : "Observation.extension",
      "path" : "Observation.extension",
      "max" : "0"
    },
    {
      "id" : "Observation.modifierExtension",
      "path" : "Observation.modifierExtension",
      "max" : "0"
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
      "id" : "Observation.identifier.value",
      "path" : "Observation.identifier.value",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;MetingID",
        "comment" : "hashed"
      }]
    },
    {
      "id" : "Observation.basedOn",
      "path" : "Observation.basedOn",
      "max" : "0"
    },
    {
      "id" : "Observation.partOf",
      "path" : "Observation.partOf",
      "max" : "0"
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
        "comment" : "hashed"
      }]
    },
    {
      "id" : "Observation.focus",
      "path" : "Observation.focus",
      "max" : "0"
    },
    {
      "id" : "Observation.encounter",
      "path" : "Observation.encounter",
      "max" : "0"
    },
    {
      "id" : "Observation.effective[x]",
      "path" : "Observation.effective[x]",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;MetingDatumTijd",
        "comment" : "dateshift"
      }]
    },
    {
      "id" : "Observation.issued",
      "path" : "Observation.issued",
      "max" : "0"
    },
    {
      "id" : "Observation.performer",
      "path" : "Observation.performer",
      "max" : "0"
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
        "map" : "AlgemeneMeting;UitslagWaardeEenheid",
        "comment" : "UCUM-code van de eenheid"
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
      "id" : "Observation.value[x]:valueRange.high.value",
      "path" : "Observation.value[x].high.value",
      "mapping" : [{
        "identity" : "sim",
        "map" : "AlgemeneMeting;ObservationValueRangeHigh"
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
        "map" : "AlgemeneMeting;UitslagDatumTijd",
        "comment" : "dateshift"
      }]
    },
    {
      "id" : "Observation.dataAbsentReason",
      "path" : "Observation.dataAbsentReason",
      "max" : "0"
    },
    {
      "id" : "Observation.interpretation",
      "path" : "Observation.interpretation",
      "max" : "0"
    },
    {
      "id" : "Observation.note",
      "path" : "Observation.note",
      "max" : "0"
    },
    {
      "id" : "Observation.bodySite",
      "path" : "Observation.bodySite",
      "max" : "0"
    },
    {
      "id" : "Observation.method",
      "path" : "Observation.method",
      "max" : "0"
    },
    {
      "id" : "Observation.specimen",
      "path" : "Observation.specimen",
      "max" : "0"
    },
    {
      "id" : "Observation.device",
      "path" : "Observation.device",
      "max" : "0"
    },
    {
      "id" : "Observation.referenceRange",
      "path" : "Observation.referenceRange",
      "max" : "0"
    },
    {
      "id" : "Observation.hasMember",
      "path" : "Observation.hasMember",
      "max" : "0"
    },
    {
      "id" : "Observation.derivedFrom",
      "path" : "Observation.derivedFrom",
      "max" : "0"
    },
    {
      "id" : "Observation.component",
      "path" : "Observation.component",
      "max" : "0"
    }]
  }
}

```
