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
  "date" : "2026-11-26T10:19:07+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Santeon FHIR R4-profiel voor de vastgelegde rookstatus van een patiënt, gebaseerd op het Santeon Informatiemodel (SIMTabakGebruik) en afgeleid van FHIR R4 Observation.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "ObservationSanTabakGebruikFromSIMTabakGebruik",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMTabakGebruik",
    "name" : "SIM Logical Model SIMTabakGebruik",
    "comment" : "Herkomst van de ObservationSanTabakGebruik-elementen in Tabakgebruik (SIMTabakGebruik), met de conversietoelichting. Afgeleid van SIMTabakGebruikToFHIR; wijzigingen alleen daar aanbrengen."
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
      "mapping" : [{
        "identity" : "ObservationSanTabakGebruikFromSIMTabakGebruik",
        "map" : "SIMTabakGebruik.MetingID",
        "comment" : "Direct: samengestelde identifier (system|value); het waardedeel wordt gehasht volgens de ETL-specificatie."
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
        "identity" : "ObservationSanTabakGebruikFromSIMTabakGebruik",
        "map" : "SIMTabakGebruik.PatientIdentificatienummer",
        "comment" : "Structuurconversie: Patient-reference op basis van identificatienummer."
      }]
    },
    {
      "id" : "Observation.effective[x]",
      "path" : "Observation.effective[x]",
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
      "id" : "Observation.effective[x]:effectiveDateTime",
      "path" : "Observation.effective[x]",
      "sliceName" : "effectiveDateTime",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "ObservationSanTabakGebruikFromSIMTabakGebruik",
        "map" : "SIMTabakGebruik.DatumVastleggen",
        "comment" : "Datatypeconversie: SIM date naar FHIR dateTime, met behoud van de oorspronkelijke datumprecisie."
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
      },
      "mapping" : [{
        "identity" : "ObservationSanTabakGebruikFromSIMTabakGebruik",
        "map" : "SIMTabakGebruik.TabakGebruikStatus",
        "comment" : "Direct: SIM CodeableConcept wordt overgenomen als FHIR CodeableConcept, gebonden aan TabakGebruikStatusCodelijst."
      }]
    }]
  }
}

```
