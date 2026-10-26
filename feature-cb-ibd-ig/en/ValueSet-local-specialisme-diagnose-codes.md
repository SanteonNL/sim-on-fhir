# IBD Specialisme-Diagnose Combinaties - SIM on FHIR v0.1.0

## ValueSet: IBD Specialisme-Diagnose Combinaties 

 **References** 

* [SpecialismeDiagnoseCode](StructureDefinition-specialisme-diagnose-code.md)

### Logical Definition (CLD)

 

### Expansion

No Expansion for this valueset (Unknown Code System)

-------

 [Description of the above table(s)](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#terminology). 



## Resource Content

```json
{
  "resourceType" : "ValueSet",
  "id" : "local-specialisme-diagnose-codes",
  "url" : "https://ig.santeon.nl/sim-on-fhir/ValueSet/local-specialisme-diagnose-codes",
  "version" : "0.1.0",
  "name" : "LocalSpecialismeDiagnoseCodes",
  "title" : "IBD Specialisme-Diagnose Combinaties",
  "status" : "active",
  "date" : "2026-10-26T15:58:19+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Specialisme-diagnose combinaties die behoren tot het IBD-cohort.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "compose" : {
    "include" : [{
      "system" : "https://ig.santeon.nl/CodeSystem/specialisme-diagnose",
      "concept" : [{
        "code" : "0313|922",
        "display" : "Interne Geneeskunde - Diagnose 922"
      },
      {
        "code" : "0313|923",
        "display" : "Interne Geneeskunde - Diagnose 923"
      },
      {
        "code" : "0318|601",
        "display" : "Maag-Darm-Leverziekten - Diagnose 601"
      },
      {
        "code" : "0318|602",
        "display" : "Maag-Darm-Leverziekten - Diagnose 602"
      },
      {
        "code" : "0303|115",
        "display" : "Chirurgie - Diagnose 115"
      },
      {
        "code" : "0303|116",
        "display" : "Chirurgie - Diagnose 116"
      },
      {
        "code" : "0303|163",
        "display" : "Chirurgie - Diagnose 163"
      },
      {
        "code" : "0303|325",
        "display" : "Chirurgie - Diagnose 325"
      },
      {
        "code" : "0303|326",
        "display" : "Chirurgie - Diagnose 326"
      }]
    }]
  }
}

```
