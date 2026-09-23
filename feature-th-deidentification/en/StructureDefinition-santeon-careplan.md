# Santeon CarePlan - Santeon CarePlan Implementation Guide v0.1.0

## Resource Profile: Santeon CarePlan 

 
Santeon-internal CarePlan profile derived from base FHIR R4. Only the elements present in the Santeon data dictionary are exposed; all other optional base elements are prohibited (0..0). 
De-identification strategy per element: 
* subject.identifier → hash
* period.start → dateshift
* period.end → dateshift
 

**Usages:**

* CapabilityStatements using this Profile: [Santeon CarePlan Server Capability Statement](CapabilityStatement-SanteonCarePlanCapabilityStatement.md)
* This Profile is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.careplan.ig|current/StructureDefinition/StructureDefinition-santeon-careplan.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-santeon-careplan.csv), [Excel](../StructureDefinition-santeon-careplan.xlsx), [Schematron](../StructureDefinition-santeon-careplan.sch) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "santeon-careplan",
  "url" : "https://ig.santeon.nl/careplan/StructureDefinition/santeon-careplan",
  "version" : "0.1.0",
  "name" : "SanteonCarePlan",
  "title" : "Santeon CarePlan",
  "status" : "draft",
  "date" : "2026-09-23T11:46:09+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Santeon-internal CarePlan profile derived from base FHIR R4.\nOnly the elements present in the Santeon data dictionary are\nexposed; all other optional base elements are prohibited (0..0).\n\nDe-identification strategy per element:\n- subject.identifier → hash\n- period.start       → dateshift\n- period.end         → dateshift",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "luscii-careplan-sql",
    "uri" : "urn:santeon:mapping:luscii-careplan-sql",
    "name" : "SQL Data Mapping (luscii_careplan.sql)",
    "comment" : "Maps CarePlan elements to luscii_careplan.sql"
  },
  {
    "identity" : "pj-careplan-sql",
    "uri" : "urn:santeon:mapping:pj-careplan-sql",
    "name" : "SQL Data Mapping (pj_careplan.sql)",
    "comment" : "Maps CarePlan elements to pj_careplan.sql"
  },
  {
    "identity" : "workflow",
    "uri" : "http://hl7.org/fhir/workflow",
    "name" : "Workflow Pattern"
  },
  {
    "identity" : "rim",
    "uri" : "http://hl7.org/v3",
    "name" : "RIM Mapping"
  },
  {
    "identity" : "w5",
    "uri" : "http://hl7.org/fhir/fivews",
    "name" : "FiveWs Pattern Mapping"
  },
  {
    "identity" : "v2",
    "uri" : "http://hl7.org/v2",
    "name" : "HL7 v2 Mapping"
  }],
  "kind" : "resource",
  "abstract" : false,
  "type" : "CarePlan",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/CarePlan",
  "derivation" : "constraint",
  "differential" : {
    "element" : [{
      "id" : "CarePlan",
      "path" : "CarePlan"
    },
    {
      "id" : "CarePlan.meta",
      "path" : "CarePlan.meta",
      "max" : "0"
    },
    {
      "id" : "CarePlan.implicitRules",
      "path" : "CarePlan.implicitRules",
      "max" : "0"
    },
    {
      "id" : "CarePlan.language",
      "path" : "CarePlan.language",
      "max" : "0"
    },
    {
      "id" : "CarePlan.text",
      "path" : "CarePlan.text",
      "max" : "0"
    },
    {
      "id" : "CarePlan.contained",
      "path" : "CarePlan.contained",
      "max" : "0"
    },
    {
      "id" : "CarePlan.extension",
      "path" : "CarePlan.extension",
      "max" : "0"
    },
    {
      "id" : "CarePlan.modifierExtension",
      "path" : "CarePlan.modifierExtension",
      "max" : "0"
    },
    {
      "id" : "CarePlan.identifier",
      "path" : "CarePlan.identifier",
      "max" : "0",
      "mapping" : [{
        "identity" : "luscii-careplan-sql",
        "map" : "luscii_patients.Patient_Number\nluscii_programs.name\nluscii_patientsprogramshistory.createdAt",
        "comment" : "Composite identifier from patient number, program, and enrollment date"
      },
      {
        "identity" : "pj-careplan-sql",
        "map" : "pj_patients.patient_ref\npj_programmes.programme_code\npj_enrolments.enrolment_date"
      }]
    },
    {
      "id" : "CarePlan.instantiatesCanonical",
      "path" : "CarePlan.instantiatesCanonical",
      "max" : "0"
    },
    {
      "id" : "CarePlan.instantiatesUri",
      "path" : "CarePlan.instantiatesUri",
      "max" : "0"
    },
    {
      "id" : "CarePlan.basedOn",
      "path" : "CarePlan.basedOn",
      "max" : "0"
    },
    {
      "id" : "CarePlan.replaces",
      "path" : "CarePlan.replaces",
      "max" : "0"
    },
    {
      "id" : "CarePlan.partOf",
      "path" : "CarePlan.partOf",
      "max" : "0"
    },
    {
      "id" : "CarePlan.category",
      "path" : "CarePlan.category",
      "min" : 1,
      "max" : "1",
      "binding" : {
        "strength" : "required",
        "description" : "Zorgplan categorieën voor het thuismonitoringprogramma Zorg Bij Jou.",
        "valueSet" : "http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.124.11.140--20240925070229"
      },
      "mapping" : [{
        "identity" : "luscii-careplan-sql",
        "map" : "luscii_programs.name",
        "comment" : "Program name serves as the category code"
      },
      {
        "identity" : "pj-careplan-sql",
        "map" : "pj_programmes.programme_code"
      }]
    },
    {
      "id" : "CarePlan.category.id",
      "path" : "CarePlan.category.id",
      "max" : "0"
    },
    {
      "id" : "CarePlan.category.coding",
      "path" : "CarePlan.category.coding",
      "min" : 1,
      "mapping" : [{
        "identity" : "luscii-careplan-sql",
        "map" : "luscii_programs.name",
        "comment" : "Program name serves as the category code"
      },
      {
        "identity" : "pj-careplan-sql",
        "map" : "pj_programmes.programme_code"
      }]
    },
    {
      "id" : "CarePlan.category.coding.id",
      "path" : "CarePlan.category.coding.id",
      "max" : "0"
    },
    {
      "id" : "CarePlan.category.coding.system",
      "path" : "CarePlan.category.coding.system",
      "min" : 1
    },
    {
      "id" : "CarePlan.category.coding.version",
      "path" : "CarePlan.category.coding.version",
      "max" : "0"
    },
    {
      "id" : "CarePlan.category.coding.code",
      "path" : "CarePlan.category.coding.code",
      "min" : 1,
      "mapping" : [{
        "identity" : "luscii-careplan-sql",
        "map" : "luscii_programs.name",
        "comment" : "Program name serves as the category code"
      },
      {
        "identity" : "pj-careplan-sql",
        "map" : "pj_programmes.programme_code"
      }]
    },
    {
      "id" : "CarePlan.category.coding.display",
      "path" : "CarePlan.category.coding.display",
      "min" : 1
    },
    {
      "id" : "CarePlan.category.coding.userSelected",
      "path" : "CarePlan.category.coding.userSelected",
      "max" : "0"
    },
    {
      "id" : "CarePlan.category.text",
      "path" : "CarePlan.category.text",
      "max" : "0"
    },
    {
      "id" : "CarePlan.title",
      "path" : "CarePlan.title",
      "max" : "0"
    },
    {
      "id" : "CarePlan.description",
      "path" : "CarePlan.description",
      "max" : "0"
    },
    {
      "id" : "CarePlan.subject",
      "path" : "CarePlan.subject",
      "type" : [{
        "code" : "Reference",
        "targetProfile" : ["http://hl7.org/fhir/StructureDefinition/Patient"]
      }],
      "mapping" : [{
        "identity" : "luscii-careplan-sql",
        "map" : "luscii_patients.Patient_Number\nluscii_programs.name\nluscii_patientsprogramshistory.createdAt",
        "comment" : "Composite identifier from patient number, program, and enrollment date"
      },
      {
        "identity" : "pj-careplan-sql",
        "map" : "pj_patients.patient_ref\npj_programmes.programme_code\npj_enrolments.enrolment_date"
      }]
    },
    {
      "id" : "CarePlan.encounter",
      "path" : "CarePlan.encounter",
      "max" : "0"
    },
    {
      "id" : "CarePlan.period",
      "path" : "CarePlan.period",
      "min" : 1,
      "mapping" : [{
        "identity" : "luscii-careplan-sql",
        "map" : "luscii_patientsprogramshistory.createdAt\nluscii_usersStatusChangeReasons.processedAt\nluscii_patientsprogramshistory.createdAt",
        "comment" : "Program enrollment date formatted as ISO 8601 UTC | Finds the earliest 'stopped' status change after program enrollment, but not beyond the next program enrollment. Returns NULL if no stop event exists (care is ongoing). Temporal constraints prevent overlapping periods."
      },
      {
        "identity" : "pj-careplan-sql",
        "map" : "pj_enrolments.enrolment_date\npj_enrolments.discharge_date"
      }]
    },
    {
      "id" : "CarePlan.period.id",
      "path" : "CarePlan.period.id",
      "max" : "0"
    },
    {
      "id" : "CarePlan.period.start",
      "path" : "CarePlan.period.start",
      "min" : 1,
      "mapping" : [{
        "identity" : "luscii-careplan-sql",
        "map" : "luscii_patientsprogramshistory.createdAt",
        "comment" : "Program enrollment date formatted as ISO 8601 UTC"
      },
      {
        "identity" : "pj-careplan-sql",
        "map" : "pj_enrolments.enrolment_date"
      }]
    },
    {
      "id" : "CarePlan.period.end",
      "path" : "CarePlan.period.end",
      "min" : 1,
      "mapping" : [{
        "identity" : "luscii-careplan-sql",
        "map" : "luscii_usersStatusChangeReasons.processedAt\nluscii_patientsprogramshistory.createdAt",
        "comment" : "Finds the earliest 'stopped' status change after program enrollment, but not beyond the next program enrollment. Returns NULL if no stop event exists (care is ongoing). Temporal constraints prevent overlapping periods."
      },
      {
        "identity" : "pj-careplan-sql",
        "map" : "pj_enrolments.discharge_date"
      }]
    },
    {
      "id" : "CarePlan.created",
      "path" : "CarePlan.created",
      "max" : "0"
    },
    {
      "id" : "CarePlan.author",
      "path" : "CarePlan.author",
      "max" : "0"
    },
    {
      "id" : "CarePlan.contributor",
      "path" : "CarePlan.contributor",
      "max" : "0"
    },
    {
      "id" : "CarePlan.careTeam",
      "path" : "CarePlan.careTeam",
      "max" : "0"
    },
    {
      "id" : "CarePlan.addresses",
      "path" : "CarePlan.addresses",
      "max" : "0"
    },
    {
      "id" : "CarePlan.supportingInfo",
      "path" : "CarePlan.supportingInfo",
      "max" : "0"
    },
    {
      "id" : "CarePlan.goal",
      "path" : "CarePlan.goal",
      "max" : "0"
    },
    {
      "id" : "CarePlan.activity",
      "path" : "CarePlan.activity",
      "max" : "0"
    },
    {
      "id" : "CarePlan.note",
      "path" : "CarePlan.note",
      "max" : "0"
    }]
  }
}

```
