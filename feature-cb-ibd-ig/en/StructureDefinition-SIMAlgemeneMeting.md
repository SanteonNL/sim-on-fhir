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
  "date" : "2026-10-16T14:27:57+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Logisch model voor een algemeneMeting binnen het Santeon Informatiemodel.",
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
    "uri" : "SIM CSVs",
    "name" : "SIM CSVs"
  },
  {
    "identity" : "SIMAlgemeneMetingToFHIR",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/observation-san",
    "name" : "FHIR"
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
      "definition" : "Logisch model voor een algemeneMeting binnen het Santeon Informatiemodel."
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
      "constraint" : [{
        "key" : "validatie-metingnaam-uitslagcode",
        "severity" : "error",
        "human" : "Valideert de relatie tussen MetingNaam en het bijbehorende type uitslag. Voor metingen met een gecodeerde uitslag moet UitslagCode afkomstig zijn uit de ValueSet die aan de betreffende MetingNaam is gekoppeld. Bijvoorbeeld: wanneer MetingNaam alcoholgebruik is (SNOMED CT 228273003), moet UitslagCode afkomstig zijn uit AlcoholGebruikStatusCodelijst. Dit geldt voor alle vastgelegde [MetingNaam-UitslagCode](<a href='https://dev.azure.com/SanteonNL/Santeon/_git/HipsETL?path=/SIM/informatiemodel%20Santeon%20valueSets%20relation.csv</a>)-combinaties. Metingen die niet in deze combinaties zijn vastgelegd, maar wel voorkomen in Santeon_MetingNaamCodelijst, hebben een UitslagWaarde als uitkomst.",
        "expression" : "(MetingNaam.coding.where(system = 'http://snomed.info/sct' and code = '228273003').exists() implies UitslagCode.memberOf('https://ig.santeon.nl/sim-on-fhir/ValueSet/AlcoholGebruikStatusCodelijst')) or (MetingNaam.coding.where(system = 'http://loinc.org' and code = '38445-3').exists() implies UitslagWaarde.exists())",
        "source" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMAlgemeneMeting"
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
        "map" : "MetingNaamCodeSysteem"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.MetingNaam.coding.code",
      "path" : "SIMAlgemeneMeting.MetingNaam.coding.code",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "MetingNaamCode"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.MetingNaam.coding.display",
      "path" : "SIMAlgemeneMeting.MetingNaam.coding.display",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "MetingNaamOmschrijving"
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
        "map" : "MetingDatumTijd"
      },
      {
        "identity" : "SIMAlgemeneMetingToFHIR",
        "map" : "Observation.effectiveDateTime"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagWaarde",
      "path" : "SIMAlgemeneMeting.UitslagWaarde",
      "short" : "Numerieke uitslag van de meting, met eenheid (UCUM) en eventueel een vergelijkingsoperator (<, <=, >=, >)",
      "definition" : "Numerieke uitslag van de meting, met eenheid (UCUM) en eventueel een vergelijkingsoperator (<, <=, >=, >)",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Quantity"
      }],
      "constraint" : [{
        "key" : "validatie-metingnaam-uitslagcode",
        "severity" : "error",
        "human" : "Valideert de relatie tussen MetingNaam en het bijbehorende type uitslag. Voor metingen met een gecodeerde uitslag moet UitslagCode afkomstig zijn uit de ValueSet die aan de betreffende MetingNaam is gekoppeld. Bijvoorbeeld: wanneer MetingNaam alcoholgebruik is (SNOMED CT 228273003), moet UitslagCode afkomstig zijn uit AlcoholGebruikStatusCodelijst. Dit geldt voor alle vastgelegde [MetingNaam-UitslagCode](<a href='https://dev.azure.com/SanteonNL/Santeon/_git/HipsETL?path=/SIM/informatiemodel%20Santeon%20valueSets%20relation.csv</a>)-combinaties. Metingen die niet in deze combinaties zijn vastgelegd, maar wel voorkomen in Santeon_MetingNaamCodelijst, hebben een UitslagWaarde als uitkomst.",
        "expression" : "(MetingNaam.coding.where(system = 'http://snomed.info/sct' and code = '228273003').exists() implies UitslagCode.memberOf('https://ig.santeon.nl/sim-on-fhir/ValueSet/AlcoholGebruikStatusCodelijst')) or (MetingNaam.coding.where(system = 'http://loinc.org' and code = '38445-3').exists() implies UitslagWaarde.exists())",
        "source" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMAlgemeneMeting"
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
        "map" : "UitslagWaarde"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagWaarde.comparator",
      "path" : "SIMAlgemeneMeting.UitslagWaarde.comparator",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "UitslagWaardeOperator"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagWaarde.system",
      "path" : "SIMAlgemeneMeting.UitslagWaarde.system",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "UitslagWaardeEenheidSysteem"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagWaarde.code",
      "path" : "SIMAlgemeneMeting.UitslagWaarde.code",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "UitslagWaardeEenheid"
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
        "map" : "ObservationValueRangeLow"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagBereik.low.unit",
      "path" : "SIMAlgemeneMeting.UitslagBereik.low.unit",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "ObservationValueRangeLowUnit"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagBereik.low.system",
      "path" : "SIMAlgemeneMeting.UitslagBereik.low.system",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "ObservationValueRangeLowSystem"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagBereik.low.code",
      "path" : "SIMAlgemeneMeting.UitslagBereik.low.code",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "ObservationValueRangeLowCode"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagBereik.high.value",
      "path" : "SIMAlgemeneMeting.UitslagBereik.high.value",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "ObservationValueRangeHigh"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagBereik.high.unit",
      "path" : "SIMAlgemeneMeting.UitslagBereik.high.unit",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "ObservationValueRangeHighUnit"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagBereik.high.system",
      "path" : "SIMAlgemeneMeting.UitslagBereik.high.system",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "ObservationValueRangeHighSystem"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagBereik.high.code",
      "path" : "SIMAlgemeneMeting.UitslagBereik.high.code",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "ObservationValueRangeHighCode"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagCode",
      "path" : "SIMAlgemeneMeting.UitslagCode",
      "short" : "Gecodeerde uitslag van de meting",
      "definition" : "Welke ValueSet van toepassing is op UitslagCode wordt bepaald door MetingNaam, zie invariants.",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }],
      "constraint" : [{
        "key" : "validatie-metingnaam-uitslagcode",
        "severity" : "error",
        "human" : "Valideert de relatie tussen MetingNaam en het bijbehorende type uitslag. Voor metingen met een gecodeerde uitslag moet UitslagCode afkomstig zijn uit de ValueSet die aan de betreffende MetingNaam is gekoppeld. Bijvoorbeeld: wanneer MetingNaam alcoholgebruik is (SNOMED CT 228273003), moet UitslagCode afkomstig zijn uit AlcoholGebruikStatusCodelijst. Dit geldt voor alle vastgelegde [MetingNaam-UitslagCode](<a href='https://dev.azure.com/SanteonNL/Santeon/_git/HipsETL?path=/SIM/informatiemodel%20Santeon%20valueSets%20relation.csv</a>)-combinaties. Metingen die niet in deze combinaties zijn vastgelegd, maar wel voorkomen in Santeon_MetingNaamCodelijst, hebben een UitslagWaarde als uitkomst.",
        "expression" : "(MetingNaam.coding.where(system = 'http://snomed.info/sct' and code = '228273003').exists() implies UitslagCode.memberOf('https://ig.santeon.nl/sim-on-fhir/ValueSet/AlcoholGebruikStatusCodelijst')) or (MetingNaam.coding.where(system = 'http://loinc.org' and code = '38445-3').exists() implies UitslagWaarde.exists())",
        "source" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMAlgemeneMeting"
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
        "map" : "UitslagCodeSysteem"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagCode.coding.code",
      "path" : "SIMAlgemeneMeting.UitslagCode.coding.code",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "UitslagCode"
      }]
    },
    {
      "id" : "SIMAlgemeneMeting.UitslagCode.coding.display",
      "path" : "SIMAlgemeneMeting.UitslagCode.coding.display",
      "mapping" : [{
        "identity" : "SIMAlgemeneMetingFromSIMCSV",
        "map" : "UitslagCodeOmschrijving"
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
        "map" : "UitslagDatumTijd"
      },
      {
        "identity" : "SIMAlgemeneMetingToFHIR",
        "map" : "Observation.valueDateTime"
      }]
    }]
  }
}

```
