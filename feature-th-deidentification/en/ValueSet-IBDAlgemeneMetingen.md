# IBDAlgemeneMetingen - SIM on FHIR v0.1.0

## ValueSet: IBDAlgemeneMetingen 

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
  "id" : "IBDAlgemeneMetingen",
  "url" : "https://ig.santeon.nl/sim-on-fhir/ValueSet/IBDAlgemeneMetingen",
  "version" : "0.1.0",
  "name" : "IBDAlgemeneMetingen",
  "title" : "IBDAlgemeneMetingen",
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
  "description" : "Metingen die worden geselecteerd voor de IBD-dataset.",
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
        "code" : "228273003"
      }]
    },
    {
      "system" : "http://loinc.org",
      "concept" : [{
        "code" : "39156-5"
      },
      {
        "code" : "8302-2"
      },
      {
        "code" : "29463-7"
      },
      {
        "code" : "38445-3"
      },
      {
        "code" : "59260-0"
      },
      {
        "code" : "787-2"
      },
      {
        "code" : "6690-2"
      },
      {
        "code" : "26464-8"
      },
      {
        "code" : "777-3"
      },
      {
        "code" : "26515-7"
      },
      {
        "code" : "1743-4"
      },
      {
        "code" : "1742-6"
      },
      {
        "code" : "77144-4"
      },
      {
        "code" : "30239-8"
      },
      {
        "code" : "1920-8"
      },
      {
        "code" : "6768-6"
      },
      {
        "code" : "77141-0"
      },
      {
        "code" : "2324-2"
      },
      {
        "code" : "1798-8"
      },
      {
        "code" : "3040-3"
      },
      {
        "code" : "14804-9"
      },
      {
        "code" : "14682-9"
      },
      {
        "code" : "77140-2"
      },
      {
        "code" : "14631-6"
      },
      {
        "code" : "62238-1"
      },
      {
        "code" : "77147-7"
      },
      {
        "code" : "1988-5"
      },
      {
        "code" : "79713-4"
      },
      {
        "code" : "93195-6"
      },
      {
        "code" : "32660-3"
      },
      {
        "code" : "32654-6"
      },
      {
        "code" : "86897-6"
      },
      {
        "code" : "72623-2"
      },
      {
        "code" : "90779-0"
      },
      {
        "code" : "74116-5"
      },
      {
        "code" : "39803-2"
      },
      {
        "code" : "74117-3"
      },
      {
        "code" : "16419-4"
      }]
    },
    {
      "system" : "https://santeon.nl/sct",
      "concept" : [{
        "code" : "MCU_E"
      },
      {
        "code" : "MCU_S"
      },
      {
        "code" : "MCD_A"
      },
      {
        "code" : "MCD_L"
      },
      {
        "code" : "MCD_B"
      },
      {
        "code" : "IC_000323"
      },
      {
        "code" : "IC_000324"
      },
      {
        "code" : "IC_000325"
      },
      {
        "code" : "IC_000326"
      },
      {
        "code" : "IC_000327"
      },
      {
        "code" : "IC_000328"
      },
      {
        "code" : "MayoEndoscopicScore"
      },
      {
        "code" : "IC_000321"
      },
      {
        "code" : "IC_000322"
      }]
    }]
  }
}

```
