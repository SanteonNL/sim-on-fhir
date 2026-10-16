# Procedure code (SAN KPI) - SIM on FHIR v0.1.0

## ValueSet: Procedure code (SAN KPI) 

 **References** 

* [Procedure (SAN KPI)](StructureDefinition-procedure-san-kpi.md)

### Logical Definition (CLD)

 

### Expansion

No Expansion for this valueset (Unknown Code System)

-------

 [Description of the above table(s)](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#terminology). 



## Resource Content

```json
{
  "resourceType" : "ValueSet",
  "id" : "procedureCode-san-kpi",
  "url" : "https://ig.santeon.nl/sim-on-fhir/ValueSet/procedureCode-san-kpi",
  "version" : "0.1.0",
  "name" : "ProcedureCodeSanKpi",
  "title" : "Procedure code (SAN KPI)",
  "status" : "draft",
  "experimental" : false,
  "date" : "2026-10-16T14:27:57+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Every code a KPI definition asks for on Procedure. The union of the ObservationDefinition codes and the value sets referenced by the Procedure criteria of all KPI definitions.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "compose" : {
    "include" : [{
      "valueSet" : ["https://ig.santeon.nl/sim-on-fhir/ValueSet/local-verrichting-codes-nza"]
    }]
  }
}

```
