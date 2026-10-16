# Procedure (SAN) - SIM on FHIR v0.1.0

## Resource Profile: Procedure (SAN) 

**Usages:**

* This Profile is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-procedure-san.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-procedure-san.csv), [Excel](../StructureDefinition-procedure-san.xlsx), [Schematron](../StructureDefinition-procedure-san.sch) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "procedure-san",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/procedure-san",
  "version" : "0.1.0",
  "name" : "ProcedureSan",
  "title" : "Procedure (SAN)",
  "status" : "draft",
  "date" : "2026-10-16T14:43:24+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "**De-identificatie:**\n- identifier → hashed\n- subject → hashed",
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
    "comment" : "Mapping van Procedure elementen naar SIM"
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
    "identity" : "v2",
    "uri" : "http://hl7.org/v2",
    "name" : "HL7 v2 Mapping"
  }],
  "kind" : "resource",
  "abstract" : false,
  "type" : "Procedure",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Procedure",
  "derivation" : "constraint",
  "differential" : {
    "element" : [{
      "id" : "Procedure",
      "path" : "Procedure"
    },
    {
      "id" : "Procedure.meta",
      "path" : "Procedure.meta",
      "max" : "0"
    },
    {
      "id" : "Procedure.implicitRules",
      "path" : "Procedure.implicitRules",
      "max" : "0"
    },
    {
      "id" : "Procedure.language",
      "path" : "Procedure.language",
      "max" : "0"
    },
    {
      "id" : "Procedure.text",
      "path" : "Procedure.text",
      "max" : "0"
    },
    {
      "id" : "Procedure.contained",
      "path" : "Procedure.contained",
      "max" : "0"
    },
    {
      "id" : "Procedure.extension",
      "path" : "Procedure.extension",
      "max" : "0"
    },
    {
      "id" : "Procedure.modifierExtension",
      "path" : "Procedure.modifierExtension",
      "max" : "0"
    },
    {
      "id" : "Procedure.identifier",
      "path" : "Procedure.identifier",
      "min" : 1,
      "max" : "1"
    },
    {
      "id" : "Procedure.identifier.use",
      "path" : "Procedure.identifier.use",
      "max" : "0"
    },
    {
      "id" : "Procedure.identifier.value",
      "path" : "Procedure.identifier.value",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "Verrichting;VerrichtingID",
        "comment" : "hashed"
      }]
    },
    {
      "id" : "Procedure.instantiatesCanonical",
      "path" : "Procedure.instantiatesCanonical",
      "max" : "0"
    },
    {
      "id" : "Procedure.instantiatesUri",
      "path" : "Procedure.instantiatesUri",
      "max" : "0"
    },
    {
      "id" : "Procedure.partOf",
      "path" : "Procedure.partOf",
      "max" : "0"
    },
    {
      "id" : "Procedure.statusReason",
      "path" : "Procedure.statusReason",
      "max" : "0"
    },
    {
      "id" : "Procedure.category",
      "path" : "Procedure.category",
      "max" : "0"
    },
    {
      "id" : "Procedure.code",
      "path" : "Procedure.code",
      "min" : 1
    },
    {
      "id" : "Procedure.code.coding",
      "path" : "Procedure.code.coding",
      "min" : 1
    },
    {
      "id" : "Procedure.code.coding.system",
      "path" : "Procedure.code.coding.system",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "Verrichting;VerrichtingTypeCodeSysteem"
      }]
    },
    {
      "id" : "Procedure.code.coding.code",
      "path" : "Procedure.code.coding.code",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "Verrichting;VerrichtingTypeCode"
      }]
    },
    {
      "id" : "Procedure.code.coding.display",
      "path" : "Procedure.code.coding.display",
      "mapping" : [{
        "identity" : "sim",
        "map" : "Verrichting;VerrichtingTypeOmschrijving"
      }]
    },
    {
      "id" : "Procedure.subject",
      "path" : "Procedure.subject",
      "type" : [{
        "code" : "Reference",
        "targetProfile" : ["https://ig.santeon.nl/sim-on-fhir/StructureDefinition/patient-san"]
      }],
      "mapping" : [{
        "identity" : "sim",
        "map" : "Verrichting;Identificatienummer",
        "comment" : "hashed"
      }]
    },
    {
      "id" : "Procedure.encounter",
      "path" : "Procedure.encounter",
      "max" : "0"
    },
    {
      "id" : "Procedure.performed[x]",
      "path" : "Procedure.performed[x]",
      "type" : [{
        "code" : "Period"
      }]
    },
    {
      "id" : "Procedure.performed[x].start",
      "path" : "Procedure.performed[x].start",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "Verrichting;VerrichtingStartDatum",
        "comment" : "dateshift"
      }]
    },
    {
      "id" : "Procedure.performed[x].end",
      "path" : "Procedure.performed[x].end",
      "mapping" : [{
        "identity" : "sim",
        "map" : "Verrichting;VerrichtingEindDatum",
        "comment" : "dateshift"
      }]
    },
    {
      "id" : "Procedure.recorder",
      "path" : "Procedure.recorder",
      "max" : "0"
    },
    {
      "id" : "Procedure.asserter",
      "path" : "Procedure.asserter",
      "max" : "0"
    },
    {
      "id" : "Procedure.location",
      "path" : "Procedure.location",
      "max" : "0"
    },
    {
      "id" : "Procedure.reasonCode",
      "path" : "Procedure.reasonCode",
      "max" : "0"
    },
    {
      "id" : "Procedure.reasonReference",
      "path" : "Procedure.reasonReference",
      "max" : "0"
    },
    {
      "id" : "Procedure.bodySite",
      "path" : "Procedure.bodySite",
      "max" : "0"
    },
    {
      "id" : "Procedure.outcome",
      "path" : "Procedure.outcome",
      "max" : "0"
    },
    {
      "id" : "Procedure.report",
      "path" : "Procedure.report",
      "max" : "0"
    },
    {
      "id" : "Procedure.complication",
      "path" : "Procedure.complication",
      "max" : "0"
    },
    {
      "id" : "Procedure.complicationDetail",
      "path" : "Procedure.complicationDetail",
      "max" : "0"
    },
    {
      "id" : "Procedure.followUp",
      "path" : "Procedure.followUp",
      "max" : "0"
    },
    {
      "id" : "Procedure.note",
      "path" : "Procedure.note",
      "max" : "0"
    },
    {
      "id" : "Procedure.focalDevice",
      "path" : "Procedure.focalDevice",
      "max" : "0"
    },
    {
      "id" : "Procedure.usedReference",
      "path" : "Procedure.usedReference",
      "max" : "0"
    },
    {
      "id" : "Procedure.usedCode",
      "path" : "Procedure.usedCode",
      "max" : "0"
    }]
  }
}

```
