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
  "date" : "2026-11-25T16:23:55+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Santeon FHIR R4-profiel voor algemene metingen, gebaseerd op het Santeon Informatiemodel (SIMAlgemeneMeting) en afgeleid van FHIR R4 Observation.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "ObservationSanFromSIMAlgemeneMeting",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMAlgemeneMeting",
    "name" : "SIM Logical Model SIMAlgemeneMeting",
    "comment" : "Herkomst van de ObservationSan-elementen in Algemene meting (SIMAlgemeneMeting), met de conversietoelichting. Afgeleid van SIMAlgemeneMetingToFHIR; wijzigingen alleen daar aanbrengen."
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
        "identity" : "ObservationSanFromSIMAlgemeneMeting",
        "map" : "SIMAlgemeneMeting.MetingID",
        "comment" : "Direct: samengestelde identifier (system|value); het waardedeel wordt gehasht volgens de ETL-specificatie."
      }]
    },
    {
      "id" : "Observation.code",
      "path" : "Observation.code",
      "mapping" : [{
        "identity" : "ObservationSanFromSIMAlgemeneMeting",
        "map" : "SIMAlgemeneMeting.MetingNaam",
        "comment" : "Direct: SIM CodeableConcept wordt overgenomen als FHIR CodeableConcept."
      }]
    },
    {
      "id" : "Observation.subject",
      "path" : "Observation.subject",
      "mapping" : [{
        "identity" : "ObservationSanFromSIMAlgemeneMeting",
        "map" : "SIMAlgemeneMeting.PatientIdentificatienummer",
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
        "identity" : "ObservationSanFromSIMAlgemeneMeting",
        "map" : "SIMAlgemeneMeting.MetingDatumTijd",
        "comment" : "Direct: SIM dateTime wordt overgenomen als FHIR dateTime."
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
      }
    },
    {
      "id" : "Observation.value[x]:valueQuantity",
      "path" : "Observation.value[x]",
      "sliceName" : "valueQuantity",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Quantity"
      }],
      "mapping" : [{
        "identity" : "ObservationSanFromSIMAlgemeneMeting",
        "map" : "SIMAlgemeneMeting.UitslagWaarde",
        "comment" : "Direct: SIM Quantity wordt overgenomen als FHIR Quantity."
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
      }],
      "mapping" : [{
        "identity" : "ObservationSanFromSIMAlgemeneMeting",
        "map" : "SIMAlgemeneMeting.UitslagBereik",
        "comment" : "Direct: SIM Range wordt overgenomen als FHIR Range."
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
      }],
      "mapping" : [{
        "identity" : "ObservationSanFromSIMAlgemeneMeting",
        "map" : "SIMAlgemeneMeting.UitslagCode",
        "comment" : "Direct: SIM CodeableConcept wordt overgenomen als FHIR CodeableConcept."
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
        "identity" : "ObservationSanFromSIMAlgemeneMeting",
        "map" : "SIMAlgemeneMeting.UitslagDatumTijd",
        "comment" : "Direct: SIM dateTime wordt overgenomen als FHIR dateTime."
      }]
    },
    {
      "id" : "Observation.method",
      "path" : "Observation.method",
      "mapping" : [{
        "identity" : "ObservationSanFromSIMAlgemeneMeting",
        "map" : "SIMAlgemeneMeting.Meetmethode",
        "comment" : "Direct: SIM CodeableConcept wordt overgenomen als FHIR CodeableConcept."
      }]
    }]
  }
}

```
