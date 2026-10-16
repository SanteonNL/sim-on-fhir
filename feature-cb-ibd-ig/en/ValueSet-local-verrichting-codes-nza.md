# Verrichting Type Codes NZa (Scopie) - SIM on FHIR v0.1.0

## ValueSet: Verrichting Type Codes NZa (Scopie) 

 **References** 

* Included into [Procedure code (SAN KPI)](ValueSet-procedureCode-san-kpi.md)

### Logical Definition (CLD)

 

### Expansion

No Expansion for this valueset (Unknown Code System)

-------

 [Description of the above table(s)](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#terminology). 



## Resource Content

```json
{
  "resourceType" : "ValueSet",
  "id" : "local-verrichting-codes-nza",
  "url" : "https://ig.santeon.nl/sim-on-fhir/ValueSet/local-verrichting-codes-nza",
  "version" : "0.1.0",
  "name" : "LocalVerrichtingCodesNZa",
  "title" : "Verrichting Type Codes NZa (Scopie)",
  "status" : "active",
  "date" : "2026-10-16T14:27:57+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "De specifieke NZa-verrichtingencodes voor de scopie-indicator.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "compose" : {
    "include" : [{
      "system" : "https://declaratie.nza.nl/zorgactiviteit",
      "concept" : [{
        "code" : "034620",
        "display" : "Verrichting 034620"
      },
      {
        "code" : "034686",
        "display" : "Verrichting 034686"
      },
      {
        "code" : "034690",
        "display" : "Verrichting 034690"
      },
      {
        "code" : "035582",
        "display" : "Verrichting 035582"
      }]
    }]
  }
}

```
