# De-identification Action Value Set - SIM on FHIR v0.1.0

## ValueSet: De-identification Action Value Set 

 **References** 

* [De-identification ruleset (model)](StructureDefinition-deidentification-ruleset.md)

### Logical Definition (CLD)

 

### Expansion

-------

 [Description of the above table(s)](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#terminology). 



## Resource Content

```json
{
  "resourceType" : "ValueSet",
  "id" : "deidentification-action-vs",
  "url" : "https://ig.santeon.nl/sim-on-fhir/ValueSet/deidentification-action-vs",
  "version" : "0.1.0",
  "name" : "DeidentificationActionVS",
  "title" : "De-identification Action Value Set",
  "status" : "active",
  "experimental" : false,
  "date" : "2026-10-26T15:58:34+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Allowed values for a de-identification rule's action.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "compose" : {
    "include" : [{
      "system" : "https://ig.santeon.nl/sim-on-fhir/CodeSystem/deidentification-action-cs"
    }]
  }
}

```
