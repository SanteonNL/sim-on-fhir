# CareplanCategoryCodelijst - SIM on FHIR v0.1.0

## ValueSet: CareplanCategoryCodelijst 

 **References** 

This value set is not used here; it may be used elsewhere (e.g. specifications and/or implementations that use this content)

### Logical Definition (CLD)

 

### Expansion

Expansions are not generated for retired value sets

-------

 [Description of the above table(s)](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#terminology). 



## Resource Content

```json
{
  "resourceType" : "ValueSet",
  "id" : "2.16.840.1.113883.2.4.3.11.60.124.11.140--20240925070229",
  "meta" : {
    "lastUpdated" : "2026-06-09T06:47:02Z",
    "profile" : ["http://hl7.org/fhir/StructureDefinition/shareablevalueset"],
    "tag" : [{
      "system" : "http://hl7.org/fhir/FHIR-version",
      "code" : "4.0.1"
    }]
  },
  "extension" : [{
    "url" : "http://hl7.org/fhir/StructureDefinition/resource-effectivePeriod",
    "valuePeriod" : {
      "start" : "2024-09-25T07:02:29Z"
    }
  },
  {
    "url" : "http://hl7.org/fhir/StructureDefinition/web-source",
    "valueUrl" : "http://decor.nictiz.nl/decor/services/RetrieveValueSet?prefix=san-gen-&version=&id=2.16.840.1.113883.2.4.3.11.60.124.11.140&effectiveDate=2024-09-25T07:02:29"
  }],
  "url" : "http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.124.11.140--20240925070229",
  "identifier" : [{
    "use" : "official",
    "system" : "urn:ietf:rfc:3986",
    "value" : "urn:oid:2.16.840.1.113883.2.4.3.11.60.124.11.140"
  }],
  "version" : "0.1.0",
  "name" : "CareplanCategoryCodelijst",
  "title" : "CareplanCategoryCodelijst",
  "status" : "retired",
  "experimental" : false,
  "date" : "2026-06-09T06:47:02Z",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "CareplanCategoryCodelijst",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "immutable" : false,
  "compose" : {
    "include" : [{
      "system" : "https://santeon.nl/sct",
      "concept" : [{
        "code" : "ZBJ_COPD",
        "display" : "ZBJ COPD"
      },
      {
        "code" : "ZBJ_Covid",
        "display" : "ZBJ Covid"
      },
      {
        "code" : "ZBJ_Zwangerschapsdiabetes",
        "display" : "ZBJ Zwangerschapsdiabetes"
      },
      {
        "code" : "ZBJ_Hartfalen",
        "display" : "ZBJ Hartfalen"
      },
      {
        "code" : "ZBJ_Astma",
        "display" : "ZBJ Astma"
      },
      {
        "code" : "ZBJ_Diabetes_type_1",
        "display" : "ZBJ Diabetes type 1"
      },
      {
        "code" : "ZBJ_Post-darm_operatie",
        "display" : "ZBJ Post-darm operatie"
      },
      {
        "code" : "ZBJ_Virtual_Fracture_Care",
        "display" : "ZBJ Virtual Fracture Care"
      },
      {
        "code" : "ZBJ_AtriumFibrilleren",
        "display" : "ZBJ AtriumFibrilleren"
      },
      {
        "code" : "ZBJ_Post-CVA",
        "display" : "ZBJ Post-CVA"
      },
      {
        "code" : "ZBJ_Perioperatief",
        "display" : "ZBJ Perioperatief"
      },
      {
        "code" : "ZBJ_IBD",
        "display" : "ZBJ IBD"
      },
      {
        "code" : "ZBJ_Diabetes",
        "display" : "ZBJ Diabetes"
      },
      {
        "code" : "ZBJ_Hypertensie",
        "display" : "ZBJ Hypertensie"
      },
      {
        "code" : "ZBJ_Geboortezorg",
        "display" : "ZBJ Geboortezorg"
      },
      {
        "code" : "CNS_conservatief",
        "display" : "CNS conservatief"
      },
      {
        "code" : "ZBJ_KnieEnHeupArtrose",
        "display" : "ZBJ Knie en Heup Artrose"
      },
      {
        "code" : "ZBJ_Nierschade",
        "display" : "ZBJ Nierschade"
      }]
    }]
  }
}

```
