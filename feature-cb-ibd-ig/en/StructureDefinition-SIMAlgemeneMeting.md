# Algemene meting (model) - SIM on FHIR v0.1.0

## Logical Model: Algemene meting (model) 

**Usages:**

* This Logical Model is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-SIMAlgemeneMeting.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-SIMAlgemeneMeting.csv), [Excel](../StructureDefinition-SIMAlgemeneMeting.xlsx) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "SIMAlgemeneMeting",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMAlgemeneMeting",
  "version" : "0.1.0",
  "name" : "SIMAlgemeneMeting",
  "title" : "Algemene meting (model)",
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
  "description" : "Generiek logisch model voor een algemene meting binnen het Santeon Informatiemodel.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "SIMAlgemeneMetingFromSIMCSV",
    "uri" : "urn:santeon:sim:csv",
    "name" : "SIM CSV"
  },
  {
    "identity" : "SIMAlgemeneMetingToFHIR",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/observation-san",
    "name" : "SIMAlgemeneMeting naar FHIR"
  }],
  "kind" : "logical",
  "abstract" : false,
  "type" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMAlgemeneMeting",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Base",
  "derivation" : "specialization",
  "differential" : {
    "element" : [{
      "id" : "SIMAlgemeneMeting",
      "path" : "SIMAlgemeneMeting",
      "short" : "Algemene meting (model)",
      "definition" : "Generiek logisch model voor een algemene meting binnen het Santeon Informatiemodel."
    },
    {
      "id" : "SIMAlgemeneMeting.MetingID",
      "path" : "SIMAlgemeneMeting.MetingID",
      "short" : "Uniek ID (key) binnen patiënt per meting",
      "definition" : "Uniek ID (key) binnen patiënt per meting",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;MetingID"
      },
      {
        "identity" : "SIMAlgemeneMetingToFHIR",
        "map" : "Observation.identifier"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.PatientIdentificatienummer",
      "path" : "SIMAlgemeneMeting.PatientIdentificatienummer",
      "short" : "Identificatienummer van de patiënt",
      "definition" : "Identificatienummer van de patiënt",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;Identificatienummer"
      },
      {
        "identity" : "SIMAlgemeneMetingToFHIR",
        "map" : "Observation.subject",
        "comment" : "Patient-reference op basis van identificatienummer."
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.MetingNaam",
      "path" : "SIMAlgemeneMeting.MetingNaam",
      "short" : "Type of naam van de meting",
      "definition" : "Type of naam van de meting",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }],
      "binding" : {
        "strength" : "required",
        "valueSet" : "https://ig.santeon.nl/sim-on-fhir/ValueSet/Santeon-MetingNaamCodelijst"
      },
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingToFHIR",
        "map" : "Observation.code"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.MetingNaam.coding.system",
      "path" : "SIMAlgemeneMeting.MetingNaam.coding.system",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;MetingNaamCodeSysteem"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.MetingNaam.coding.code",
      "path" : "SIMAlgemeneMeting.MetingNaam.coding.code",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;MetingNaamCode"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.MetingNaam.coding.display",
      "path" : "SIMAlgemeneMeting.MetingNaam.coding.display",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;MetingNaamOmschrijving"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.MetingDatumTijd",
      "path" : "SIMAlgemeneMeting.MetingDatumTijd",
      "short" : "Datum en eventueel tijd die het dichtst ligt bij het daadwerkelijke meetmoment of de observatie",
      "definition" : "Datum en eventueel tijd die het dichtst ligt bij het daadwerkelijke meetmoment of de observatie",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;MetingDatumTijd"
      },
      {
        "identity" : "SIMAlgemeneMetingToFHIR",
        "map" : "Observation.effectiveDateTime"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagWaarde",
      "path" : "SIMAlgemeneMeting.UitslagWaarde",
      "short" : "Numerieke uitslag van de meting, met eenheid (UCUM) en eventueel een vergelijkingsoperator",
      "definition" : "Numerieke uitslag van de meting, met eenheid (UCUM) en eventueel een vergelijkingsoperator",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Quantity"
      }],
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingToFHIR",
        "map" : "Observation.valueQuantity"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagWaarde.value",
      "path" : "SIMAlgemeneMeting.UitslagWaarde.value",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;UitslagWaarde"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagWaarde.comparator",
      "path" : "SIMAlgemeneMeting.UitslagWaarde.comparator",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;UitslagWaardeOperator"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagWaarde.system",
      "path" : "SIMAlgemeneMeting.UitslagWaarde.system",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;UitslagWaardeEenheidSysteem"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagWaarde.code",
      "path" : "SIMAlgemeneMeting.UitslagWaarde.code",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;UitslagWaardeEenheid"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagBereik",
      "path" : "SIMAlgemeneMeting.UitslagBereik",
      "short" : "Uitslag als bereik: een ondergrens en/of een bovengrens, beide inclusief",
      "definition" : "Uitslag als bereik: een ondergrens en/of een bovengrens, beide inclusief",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Range"
      }],
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingToFHIR",
        "map" : "Observation.valueRange"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagBereik.low.value",
      "path" : "SIMAlgemeneMeting.UitslagBereik.low.value",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;ObservationValueRangeLow"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagBereik.low.unit",
      "path" : "SIMAlgemeneMeting.UitslagBereik.low.unit",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;ObservationValueRangeLowUnit"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagBereik.low.system",
      "path" : "SIMAlgemeneMeting.UitslagBereik.low.system",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;ObservationValueRangeLowSystem"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagBereik.low.code",
      "path" : "SIMAlgemeneMeting.UitslagBereik.low.code",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;ObservationValueRangeLowCode"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagBereik.high.value",
      "path" : "SIMAlgemeneMeting.UitslagBereik.high.value",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;ObservationValueRangeHigh"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagBereik.high.unit",
      "path" : "SIMAlgemeneMeting.UitslagBereik.high.unit",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;ObservationValueRangeHighUnit"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagBereik.high.system",
      "path" : "SIMAlgemeneMeting.UitslagBereik.high.system",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;ObservationValueRangeHighSystem"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagBereik.high.code",
      "path" : "SIMAlgemeneMeting.UitslagBereik.high.code",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;ObservationValueRangeHighCode"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagCode",
      "path" : "SIMAlgemeneMeting.UitslagCode",
      "short" : "Gecodeerde uitslag van de meting",
      "definition" : "Welke ValueSet van toepassing is op UitslagCode wordt bepaald door MetingNaam. De bijbehorende terminologierelaties worden afzonderlijk beheerd.",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }],
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingToFHIR",
        "map" : "Observation.valueCodeableConcept"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagCode.coding.system",
      "path" : "SIMAlgemeneMeting.UitslagCode.coding.system",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;UitslagCodeSysteem"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagCode.coding.code",
      "path" : "SIMAlgemeneMeting.UitslagCode.coding.code",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;UitslagCode"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagCode.coding.display",
      "path" : "SIMAlgemeneMeting.UitslagCode.coding.display",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;UitslagCodeOmschrijving"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagDatumTijd",
      "path" : "SIMAlgemeneMeting.UitslagDatumTijd",
      "short" : "Datum en eventueel tijd als antwoord op een vraag, bijvoorbeeld: sinds wanneer rookt u? Een vage datumtijd (alleen jaar of datum) is niet wenselijk maar wel toegestaan.",
      "definition" : "Datum en eventueel tijd als antwoord op een vraag, bijvoorbeeld: sinds wanneer rookt u? Een vage datumtijd (alleen jaar of datum) is niet wenselijk maar wel toegestaan.",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;UitslagDatumTijd"
      },
      {
        "identity" : "SIMAlgemeneMetingToFHIR",
        "map" : "Observation.valueDateTime"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.Meetmethode",
      "path" : "SIMAlgemeneMeting.Meetmethode",
      "short" : "Methode waarmee de meting is uitgevoerd",
      "definition" : "Methode waarmee de meting is uitgevoerd",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }],
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingToFHIR",
        "map" : "Observation.method"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.Meetmethode.coding.system",
      "path" : "SIMAlgemeneMeting.Meetmethode.coding.system",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;MeetmethodeCodeSysteem"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.Meetmethode.coding.code",
      "path" : "SIMAlgemeneMeting.Meetmethode.coding.code",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;MeetmethodeCode"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.Meetmethode.coding.display",
      "path" : "SIMAlgemeneMeting.Meetmethode.coding.display",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "AlgemeneMeting;MeetmethodeOmschrijving"
      }]
    }]
  }
}

```
