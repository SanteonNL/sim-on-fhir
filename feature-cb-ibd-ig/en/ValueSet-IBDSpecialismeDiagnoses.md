# IBD Specialisme Diagnoses - SIM on FHIR v0.1.0

## ValueSet: IBD Specialisme Diagnoses 

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
  "id" : "IBDSpecialismeDiagnoses",
  "url" : "https://ig.santeon.nl/sim-on-fhir/ValueSet/IBDSpecialismeDiagnoses",
  "version" : "0.1.0",
  "name" : "IBDSpecialismeDiagnoses",
  "title" : "IBD Specialisme Diagnoses",
  "status" : "draft",
  "date" : "2026-10-26T15:58:19+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Specialisme-diagnosecombinaties die worden geselecteerd voor de IBD-dataset.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "compose" : {
    "include" : [{
      "system" : "http://fhir.nl/fhir/NamingSystem/nza",
      "concept" : [{
        "code" : "0324|101"
      },
      {
        "code" : "0324|102"
      },
      {
        "code" : "0324|105"
      },
      {
        "code" : "0324|107"
      },
      {
        "code" : "0324|115"
      },
      {
        "code" : "0324|116"
      },
      {
        "code" : "0324|117"
      },
      {
        "code" : "0324|201"
      },
      {
        "code" : "0324|203"
      },
      {
        "code" : "0303|115"
      },
      {
        "code" : "0303|116"
      },
      {
        "code" : "0303|129"
      },
      {
        "code" : "0303|163"
      },
      {
        "code" : "0303|325"
      },
      {
        "code" : "0303|326"
      },
      {
        "code" : "0303|333"
      },
      {
        "code" : "0303|334"
      },
      {
        "code" : "0318|601"
      },
      {
        "code" : "0318|602"
      },
      {
        "code" : "0318|610"
      },
      {
        "code" : "0313|922"
      },
      {
        "code" : "0313|923"
      },
      {
        "code" : "0313|927"
      },
      {
        "code" : "0316|3303"
      },
      {
        "code" : "0316|3308"
      },
      {
        "code" : "0316|3311"
      },
      {
        "code" : "0316|3314"
      },
      {
        "code" : "0316|3315"
      },
      {
        "code" : "0316|3399"
      },
      {
        "code" : "*|00"
      },
      {
        "code" : "*|000"
      },
      {
        "code" : "*|0000"
      }]
    }]
  }
}

```
