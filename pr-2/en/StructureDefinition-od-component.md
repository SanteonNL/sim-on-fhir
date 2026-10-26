# ObservationDefinition Component - SIM on FHIR v0.1.0

## Extension: ObservationDefinition Component 

**Context of Use**

**Usage info**

**Usages:**

* This Extension is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-od-component.json)

### Formal Views of Extension Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-od-component.csv), [Excel](../StructureDefinition-od-component.xlsx), [Schematron](../StructureDefinition-od-component.sch) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "od-component",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/od-component",
  "version" : "0.1.0",
  "name" : "ODComponent",
  "title" : "ObservationDefinition Component",
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
  "description" : "Points from a panel ObservationDefinition (e.g. blood pressure) to the ObservationDefinition of one of its components (e.g. systolic). The component's own code and unit become a constraint on Observation.component in the generated profile.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "rim",
    "uri" : "http://hl7.org/v3",
    "name" : "RIM Mapping"
  }],
  "kind" : "complex-type",
  "abstract" : false,
  "context" : [{
    "type" : "element",
    "expression" : "ObservationDefinition"
  }],
  "type" : "Extension",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Extension",
  "derivation" : "constraint",
  "differential" : {
    "element" : [{
      "id" : "Extension",
      "path" : "Extension",
      "short" : "ObservationDefinition Component",
      "definition" : "Points from a panel ObservationDefinition (e.g. blood pressure) to the ObservationDefinition of one of its components (e.g. systolic). The component's own code and unit become a constraint on Observation.component in the generated profile."
    },
    {
      "id" : "Extension.extension",
      "path" : "Extension.extension",
      "max" : "0"
    },
    {
      "id" : "Extension.url",
      "path" : "Extension.url",
      "fixedUri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/od-component"
    },
    {
      "id" : "Extension.value[x]",
      "path" : "Extension.value[x]",
      "type" : [{
        "code" : "Reference",
        "targetProfile" : ["http://hl7.org/fhir/StructureDefinition/ObservationDefinition"]
      }]
    }]
  }
}

```
