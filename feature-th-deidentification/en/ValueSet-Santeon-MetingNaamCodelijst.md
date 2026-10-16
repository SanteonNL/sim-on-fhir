# Santeon_MetingNaamCodelijst - SIM on FHIR v0.1.0

## ValueSet: Santeon_MetingNaamCodelijst 

 **References** 

* [Algemene meting (model)](StructureDefinition-SIMAlgemeneMeting.md)

### Logical Definition (CLD)

 

### Expansion

-------

 [Description of the above table(s)](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#terminology). 



## Resource Content

```json
{
  "resourceType" : "ValueSet",
  "id" : "Santeon-MetingNaamCodelijst",
  "url" : "https://ig.santeon.nl/sim-on-fhir/ValueSet/Santeon-MetingNaamCodelijst",
  "version" : "0.1.0",
  "name" : "Santeon_MetingNaamCodelijst",
  "title" : "Santeon_MetingNaamCodelijst",
  "status" : "draft",
  "date" : "2026-10-16T14:43:24+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "ValueSet met metingnamen die binnen het Santeon Informatiemodel worden gebruikt.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "compose" : {
    "include" : [{
      "system" : "http://loinc.org",
      "concept" : [{
        "code" : "38445-3",
        "display" : "Calprotectin [Mass/volume] in Stool"
      }]
    },
    {
      "system" : "http://snomed.info/sct",
      "concept" : [{
        "code" : "228273003"
      }]
    }]
  }
}

```
