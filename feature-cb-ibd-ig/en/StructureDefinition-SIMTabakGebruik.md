# Tabakgebruik (model) - SIM on FHIR v0.1.0

## Logical Model: Tabakgebruik (model) 

**Usages:**

* This Logical Model is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-SIMTabakGebruik.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-SIMTabakGebruik.csv), [Excel](../StructureDefinition-SIMTabakGebruik.xlsx) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "SIMTabakGebruik",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMTabakGebruik",
  "version" : "0.1.0",
  "name" : "SIMTabakGebruik",
  "title" : "Tabakgebruik (model)",
  "status" : "draft",
  "date" : "2026-11-25T16:23:53+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Generiek logisch model voor de vastgelegde rookstatus van een patiënt binnen het Santeon Informatiemodel.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "SIMTabakGebruikFromSIMCSV",
    "uri" : "urn:santeon:sim:csv",
    "name" : "SIM CSV",
    "comment" : "Herleidbaarheid van SIMTabakGebruik-elementen naar de oorspronkelijke SIM CSV-attributen."
  },
  {
    "identity" : "SIMTabakGebruikToFHIR",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/observation-san-tabakgebruik",
    "name" : "FHIR-mapping naar ObservationSanTabakGebruik",
    "comment" : "Mapping van SIMTabakGebruik-elementen naar het Santeon FHIR-profiel ObservationSanTabakGebruik, inclusief harmonisatie met FHIR-standaarden en toelichting op benodigde conversies."
  }],
  "kind" : "logical",
  "abstract" : false,
  "type" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMTabakGebruik",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Base",
  "derivation" : "specialization",
  "differential" : {
    "element" : [{
      "id" : "SIMTabakGebruik",
      "path" : "SIMTabakGebruik",
      "short" : "Tabakgebruik (model)",
      "definition" : "Generiek logisch model voor de vastgelegde rookstatus van een patiënt binnen het Santeon Informatiemodel."
    },
    {
      "id" : "SIMTabakGebruik.MetingID",
      "path" : "SIMTabakGebruik.MetingID",
      "short" : "Uniek ID (key) binnen patiënt per meting",
      "definition" : "Uniek ID (key) binnen patiënt per meting",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMTabakGebruikFromSIMCSV",
        "map" : "TabakGebruik;MetingID"
      },
      {
        "identity" : "SIMTabakGebruikToFHIR",
        "map" : "Observation.identifier",
        "comment" : "Direct: samengestelde identifier (system|value); het waardedeel wordt gehasht volgens de ETL-specificatie."
      }]
    },
    {
      "id" : "SIMTabakGebruik.PatientIdentificatienummer",
      "path" : "SIMTabakGebruik.PatientIdentificatienummer",
      "short" : "Identificatienummer van de patiënt",
      "definition" : "Identificatienummer van de patiënt",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMTabakGebruikFromSIMCSV",
        "map" : "TabakGebruik;Identificatienummer"
      },
      {
        "identity" : "SIMTabakGebruikToFHIR",
        "map" : "Observation.subject",
        "comment" : "Structuurconversie: Patient-reference op basis van identificatienummer."
      }]
    },
    {
      "id" : "SIMTabakGebruik.TabakGebruikStatus",
      "path" : "SIMTabakGebruik.TabakGebruikStatus",
      "short" : "Rookstatus van de patiënt",
      "definition" : "Rookstatus van de patiënt",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }],
      "binding" : {
        "strength" : "extensible",
        "valueSet" : "https://ig.santeon.nl/sim-on-fhir/ValueSet/TabakGebruikStatusCodelijst"
      },
      "mapping" : [{
        "identity" : "SIMTabakGebruikToFHIR",
        "map" : "Observation.valueCodeableConcept",
        "comment" : "Direct: SIM CodeableConcept wordt overgenomen als FHIR CodeableConcept, gebonden aan TabakGebruikStatusCodelijst."
      }]
    },
    {
      "id" : "SIMTabakGebruik.TabakGebruikStatus.coding.system",
      "path" : "SIMTabakGebruik.TabakGebruikStatus.coding.system",
      "mapping" : [{
        "identity" : "SIMTabakGebruikFromSIMCSV",
        "map" : "TabakGebruik;TabakGebruikStatusSysteem"
      }]
    },
    {
      "id" : "SIMTabakGebruik.TabakGebruikStatus.coding.code",
      "path" : "SIMTabakGebruik.TabakGebruikStatus.coding.code",
      "mapping" : [{
        "identity" : "SIMTabakGebruikFromSIMCSV",
        "map" : "TabakGebruik;TabakGebruikStatus"
      }]
    },
    {
      "id" : "SIMTabakGebruik.TabakGebruikStatus.coding.display",
      "path" : "SIMTabakGebruik.TabakGebruikStatus.coding.display",
      "mapping" : [{
        "identity" : "SIMTabakGebruikFromSIMCSV",
        "map" : "TabakGebruik;TabakGebruikStatusOmschrijving"
      }]
    },
    {
      "id" : "SIMTabakGebruik.DatumVastleggen",
      "path" : "SIMTabakGebruik.DatumVastleggen",
      "short" : "Datum waarop de rookstatus is vastgelegd",
      "definition" : "Datum waarop de rookstatus is vastgelegd",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "date"
      }],
      "mapping" : [{
        "identity" : "SIMTabakGebruikFromSIMCSV",
        "map" : "TabakGebruik;TabakGebruikStatusDatumVastleggen"
      },
      {
        "identity" : "SIMTabakGebruikToFHIR",
        "map" : "Observation.effectiveDateTime",
        "comment" : "Datatypeconversie: SIM date naar FHIR dateTime, met behoud van de oorspronkelijke datumprecisie."
      }]
    }]
  }
}

```
