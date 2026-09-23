# Voorbeeld Santeon CarePlan (CKD conservatief) - Santeon CarePlan Implementation Guide v0.1.0

## Example CarePlan: Voorbeeld Santeon CarePlan (CKD conservatief)

**status**: Draft

**intent**: Proposal

**category**: Chronische Nierschade

**subject**: [Anonymous Patient (no stated gender), DoB Unknown ( https://ig.santeon.nl/fhir/NamingSystem/patient-id#HASH-7F2A91C4)](Patient-ExamplePatient.md)

**period**: 2025-01-15 --> 2025-12-31



## Resource Content

```json
{
  "resourceType" : "CarePlan",
  "id" : "SanteonCarePlanExample",
  "status" : "draft",
  "intent" : "proposal",
  "category" : [{
    "coding" : [{
      "system" : "https://ig.santeon.nl/careplan/CodeSystem/santeon-careplan-category-cs",
      "code" : "ckd",
      "display" : "Chronische Nierschade"
    }]
  }],
  "subject" : {
    "reference" : "Patient/ExamplePatient"
  },
  "period" : {
    "start" : "2025-01-15",
    "end" : "2025-12-31"
  }
}

```
