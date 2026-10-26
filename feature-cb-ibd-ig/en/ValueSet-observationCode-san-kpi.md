# Observation code (SAN KPI) - SIM on FHIR v0.1.0

## ValueSet: Observation code (SAN KPI) 

 **References** 

* [Observation (SAN KPI)](StructureDefinition-observation-san-kpi.md)

### Logical Definition (CLD)

 

### Expansion

-------

 [Description of the above table(s)](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#terminology). 



## Resource Content

```json
{
  "resourceType" : "ValueSet",
  "id" : "observationCode-san-kpi",
  "url" : "https://ig.santeon.nl/sim-on-fhir/ValueSet/observationCode-san-kpi",
  "version" : "0.1.0",
  "name" : "ObservationCodeSanKpi",
  "title" : "Observation code (SAN KPI)",
  "status" : "draft",
  "experimental" : false,
  "date" : "2026-10-26T15:58:19+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Every code a KPI definition asks for on Observation. The union of the ObservationDefinition codes and the value sets referenced by the Observation criteria of all KPI definitions.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "compose" : {
    "include" : [{
      "system" : "http://snomed.info/sct",
      "concept" : [{
        "code" : "228273003",
        "display" : "Finding relating to alcohol drinking behavior (finding)"
      }]
    },
    {
      "system" : "http://loinc.org",
      "concept" : [{
        "code" : "38445-3",
        "display" : "Calprotectin [Mass/volume] in Stool"
      }]
    }]
  }
}

```
