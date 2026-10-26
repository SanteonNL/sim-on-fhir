# IBD Zorgtype Codes (11 en 21) - SIM on FHIR v0.1.0

## ValueSet: IBD Zorgtype Codes (11 en 21) 

 **References** 

This value set is not used here; it may be used elsewhere (e.g. specifications and/or implementations that use this content)

### Logical Definition (CLD)

 

### Expansion

No Expansion for this valueset (Unknown Code System)

-------

 [Description of the above table(s)](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#terminology). 



## Resource Content

```json
{
  "resourceType" : "ValueSet",
  "id" : "local-zorgtype-codes",
  "url" : "https://ig.santeon.nl/sim-on-fhir/ValueSet/local-zorgtype-codes",
  "version" : "0.1.0",
  "name" : "LocalZorgtypeCodes",
  "title" : "IBD Zorgtype Codes (11 en 21)",
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
  "description" : "Bevat de toegestane landelijke zorgtypes 11 en 21.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "compose" : {
    "include" : [{
      "system" : "https://declaratie.nza.nl/zorgtype",
      "concept" : [{
        "code" : "11",
        "display" : "Regulier DBC-zorgtraject"
      },
      {
        "code" : "21",
        "display" : "Regulier vervolg-DBC-zorgtraject"
      }]
    }]
  }
}

```
