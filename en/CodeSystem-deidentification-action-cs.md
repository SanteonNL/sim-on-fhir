# De-identification Action Code System - Santeon CarePlan Implementation Guide v0.1.0

## CodeSystem: De-identification Action Code System 

 
The transformations a de-identification rule may apply to an element. 

This Code system is referenced in the definition of the following value sets:

* [De-identification Action Value Set](ValueSet-deidentification-action-vs.md)

-------

 [Description of the above table(s)](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#terminology). 



## Resource Content

```json
{
  "resourceType" : "CodeSystem",
  "id" : "deidentification-action-cs",
  "url" : "https://ig.santeon.nl/careplan/CodeSystem/deidentification-action-cs",
  "version" : "0.1.0",
  "name" : "DeidentificationActionCS",
  "title" : "De-identification Action Code System",
  "status" : "active",
  "experimental" : false,
  "date" : "2026-09-23T11:47:11+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "The transformations a de-identification rule may apply to an element.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "caseSensitive" : true,
  "content" : "complete",
  "count" : 5,
  "concept" : [{
    "code" : "none",
    "display" : "None",
    "definition" : "Export the element unchanged. Requires an exceptionReason."
  },
  {
    "code" : "hash",
    "display" : "Hash",
    "definition" : "Replace the value with an HMAC hash."
  },
  {
    "code" : "shift",
    "display" : "Shift",
    "definition" : "Move a date/dateTime by a per-patient offset."
  },
  {
    "code" : "clamp-age",
    "display" : "Clamp age",
    "definition" : "Bound the age implied by a birth date to a range."
  },
  {
    "code" : "first-of-month",
    "display" : "First of month",
    "definition" : "Floor a date to the first day of its month."
  }]
}

```
