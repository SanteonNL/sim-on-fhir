# SpecialismeDiagnoseCode - SIM on FHIR v0.1.0

## Extension: SpecialismeDiagnoseCode 

**Context of Use**

**Usage info**

**Usages:**

* This Extension is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-specialisme-diagnose-code.json)

### Formal Views of Extension Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-specialisme-diagnose-code.csv), [Excel](../StructureDefinition-specialisme-diagnose-code.xlsx), [Schematron](../StructureDefinition-specialisme-diagnose-code.sch) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "specialisme-diagnose-code",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/specialisme-diagnose-code",
  "version" : "0.1.0",
  "name" : "SpecialismeDiagnoseCode",
  "title" : "SpecialismeDiagnoseCode",
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
  "description" : "(Tijdelijke) Extensie voor SpecialismeDiagnoseCode. Deze informatie dient in een toekomstige FHIR-conforme modellering te worden ondergebracht in Condition.code.",
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
    "uri" : "http://hl7.org/v3",
    "name" : "Santeon Informatie Model",
    "comment" : "Mapping van Extensie elementen naar SIM"
  }],
  "kind" : "complex-type",
  "abstract" : false,
  "context" : [{
    "type" : "element",
    "expression" : "Element"
  }],
  "type" : "Extension",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Extension",
  "derivation" : "constraint",
  "differential" : {
    "element" : [{
      "id" : "Extension",
      "path" : "Extension",
      "short" : "SpecialismeDiagnoseCode",
      "definition" : "(Tijdelijke) Extensie voor SpecialismeDiagnoseCode. Deze informatie dient in een toekomstige FHIR-conforme modellering te worden ondergebracht in Condition.code."
    },
    {
      "id" : "Extension.extension",
      "path" : "Extension.extension",
      "max" : "0"
    },
    {
      "id" : "Extension.url",
      "path" : "Extension.url",
      "fixedUri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/specialisme-diagnose-code"
    },
    {
      "id" : "Extension.value[x]",
      "path" : "Extension.value[x]",
      "type" : [{
        "code" : "CodeableConcept"
      }],
      "binding" : {
        "strength" : "required",
        "valueSet" : "https://ig.santeon.nl/sim-on-fhir/ValueSet/local-specialisme-diagnose-codes"
      }
    },
    {
      "id" : "Extension.value[x].coding",
      "path" : "Extension.value[x].coding",
      "min" : 1
    },
    {
      "id" : "Extension.value[x].coding.system",
      "path" : "Extension.value[x].coding.system",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "DBC;SpecialismeDiagnoseCodeSysteem"
      }]
    },
    {
      "id" : "Extension.value[x].coding.code",
      "path" : "Extension.value[x].coding.code",
      "min" : 1,
      "mapping" : [{
        "identity" : "sim",
        "map" : "DBC;SpecialismeDiagnoseCode"
      }]
    },
    {
      "id" : "Extension.value[x].coding.display",
      "path" : "Extension.value[x].coding.display",
      "mapping" : [{
        "identity" : "sim",
        "map" : "DBC;SpecialismeDiagnoseOmschrijving"
      }]
    }]
  }
}

```
