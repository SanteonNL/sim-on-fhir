# AlcoholGebruikStatusCodelijst - SIM on FHIR v0.1.0

## ValueSet: AlcoholGebruikStatusCodelijst 

 **References** 

This value set is not used here; it may be used elsewhere (e.g. specifications and/or implementations that use this content)

### Logical Definition (CLD)

 

### Expansion

-------

 [Description of the above table(s)](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#terminology). 



## Resource Content

```json
{
  "resourceType" : "ValueSet",
  "id" : "AlcoholGebruikStatusCodelijst",
  "url" : "https://ig.santeon.nl/sim-on-fhir/ValueSet/AlcoholGebruikStatusCodelijst",
  "version" : "0.1.0",
  "name" : "AlcoholGebruikStatusCodelijst",
  "title" : "AlcoholGebruikStatusCodelijst",
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
  "description" : "Codelijst voor de gecodeerde uitslag van alcoholgebruik.",
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
        "display" : "Anders"
      }]
    },
    {
      "system" : "http://snomed.info/sct",
      "concept" : [{
        "code" : "219006",
        "display" : "drinkt alcohol"
      },
      {
        "code" : "105542008",
        "display" : "drinkt momenteel geen alcohol"
      },
      {
        "code" : "82581004",
        "display" : "dronk vroeger alcohol"
      },
      {
        "code" : "783261004",
        "display" : "levenslange niet-drinker van alcohol"
      }]
    }]
  }
}

```
