# Santeon_MetingNaamCodelijst - SIM on FHIR v0.1.0

## ValueSet: Santeon_MetingNaamCodelijst 

 **References** 

* [Algemene meting (model)](StructureDefinition-SIMAlgemeneMeting.md)
* [Observation (SAN)](StructureDefinition-observation-san.md)

### Logical Definition (CLD)

 

### Expansion

No Expansion for this valueset (Unknown Code System)

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
  "date" : "2026-11-11T17:43:54+00:00",
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
      },
      {
        "code" : "39156-5"
      },
      {
        "code" : "29463-7"
      },
      {
        "code" : "8302-2"
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
        "code" : "1988-5"
      },
      {
        "code" : "93195-6"
      },
      {
        "code" : "79713-4"
      },
      {
        "code" : "86897-6"
      },
      {
        "code" : "90779-0"
      },
      {
        "code" : "39803-2"
      },
      {
        "code" : "74117-3"
      }]
    },
    {
      "system" : "http://snomed.info/sct",
      "concept" : [{
        "code" : "228273003"
      },
      {
        "code" : "365980008"
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
        "code" : "MCD_B"
      },
      {
        "code" : "MCD_L"
      },
      {
        "code" : "MayoEndoscopicScore"
      }]
    }]
  }
}

```
