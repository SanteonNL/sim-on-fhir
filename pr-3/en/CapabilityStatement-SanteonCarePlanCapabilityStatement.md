# Santeon CarePlan Server Capability Statement - Santeon CarePlan Implementation Guide v0.1.0

## CapabilityStatement: Santeon CarePlan Server Capability Statement (Experimental) 

 
Describes the FHIR server capabilities for the Santeon CarePlan IG. 

 [Raw OpenAPI-Swagger Definition file](../SanteonCarePlanCapabilityStatement.openapi.json) | [Download](../SanteonCarePlanCapabilityStatement.openapi.json) 



## Resource Content

```json
{
  "resourceType" : "CapabilityStatement",
  "id" : "SanteonCarePlanCapabilityStatement",
  "url" : "https://ig.santeon.nl/careplan/CapabilityStatement/SanteonCarePlanCapabilityStatement",
  "version" : "0.1.0",
  "name" : "SanteonCarePlanCapabilityStatement",
  "title" : "Santeon CarePlan Server Capability Statement",
  "status" : "draft",
  "experimental" : true,
  "date" : "2025-01-01",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Describes the FHIR server capabilities for the Santeon CarePlan IG.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "kind" : "requirements",
  "fhirVersion" : "4.0.1",
  "format" : ["json", "xml"],
  "rest" : [{
    "mode" : "server",
    "resource" : [{
      "type" : "CarePlan",
      "profile" : "https://ig.santeon.nl/careplan/StructureDefinition/santeon-careplan",
      "interaction" : [{
        "code" : "read"
      },
      {
        "code" : "search-type"
      },
      {
        "code" : "create"
      },
      {
        "code" : "update"
      }],
      "searchParam" : [{
        "name" : "status",
        "definition" : "http://hl7.org/fhir/SearchParameter/CarePlan-status",
        "type" : "token"
      },
      {
        "name" : "category",
        "definition" : "http://hl7.org/fhir/SearchParameter/CarePlan-category",
        "type" : "token"
      },
      {
        "name" : "patient",
        "definition" : "http://hl7.org/fhir/SearchParameter/clinical-patient",
        "type" : "reference"
      },
      {
        "name" : "date",
        "definition" : "http://hl7.org/fhir/SearchParameter/clinical-date",
        "type" : "date"
      }]
    }]
  }]
}

```
