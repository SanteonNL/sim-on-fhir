# TabakGebruikStatusCodelijst - SIM on FHIR v0.1.0

## ValueSet: TabakGebruikStatusCodelijst 

 **References** 

* [Tabakgebruik (model)](StructureDefinition-SIMTabakGebruik.md)
* [Observation TabakGebruik (SAN)](StructureDefinition-observation-san-tabakgebruik.md)

### Logical Definition (CLD)

 

### Expansion

-------

 [Description of the above table(s)](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#terminology). 



## Resource Content

```json
{
  "resourceType" : "ValueSet",
  "id" : "TabakGebruikStatusCodelijst",
  "url" : "https://ig.santeon.nl/sim-on-fhir/ValueSet/TabakGebruikStatusCodelijst",
  "version" : "0.1.0",
  "name" : "TabakGebruikStatusCodelijst",
  "title" : "TabakGebruikStatusCodelijst",
  "status" : "draft",
  "date" : "2026-11-11T17:38:18+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Codelijst voor de rookstatus van een patiënt, zoals gebruikt in het Santeon Informatiemodel (SIM-versie 2024-04-24).",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "compose" : {
    "include" : [{
      "system" : "http://terminology.hl7.org/CodeSystem/v3-NullFlavor",
      "version" : "4.0.0",
      "concept" : [{
        "code" : "OTH",
        "display" : "Other"
      }]
    },
    {
      "system" : "http://snomed.info/sct",
      "concept" : [{
        "code" : "449868002",
        "display" : "Rookt dagelijks tabak"
      },
      {
        "code" : "428041000124106",
        "display" : "Rookt soms tabak"
      },
      {
        "code" : "43381005",
        "display" : "Rookt passief tabak"
      },
      {
        "code" : "8517006",
        "display" : "Rookte vroeger"
      },
      {
        "code" : "405746006",
        "display" : "Niet-roker, maar rookgedrag in verleden onbekend"
      },
      {
        "code" : "266919005",
        "display" : "Heeft nooit gerookt"
      }]
    }]
  }
}

```
