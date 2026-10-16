# IBD cohort (generated) - SIM on FHIR v0.1.0

## Group: IBD cohort (generated)

* Metadata: Type
  * ?: Person
* Metadata: Actual
  * ?: true
* Metadata: Generated using version 0.5.6 of the sample-content-ig Liquid templates



## Resource Content

```json
{
  "resourceType" : "Group",
  "id" : "ibd-dataset-cohort",
  "modifierExtension" : [{
    "url" : "http://hl7.org/fhir/uv/bulkdata/StructureDefinition/member-filter",
    "valueExpression" : {
      "language" : "application/x-fhir-query",
      "expression" : "Patient?birthdate=le{today-18y}"
    }
  },
  {
    "url" : "http://hl7.org/fhir/uv/bulkdata/StructureDefinition/member-filter",
    "valueExpression" : {
      "language" : "application/x-fhir-query",
      "expression" : "EpisodeOfCare?type:in=https://ig.santeon.nl/sim-on-fhir/ValueSet/local-zorgtype-codes&status=active&date=ge2018-01-01&diagnosis:Condition.code:in=https://ig.santeon.nl/sim-on-fhir/ValueSet/local-specialisme-diagnose-codes"
    }
  }],
  "type" : "person",
  "actual" : true,
  "name" : "IBD cohort"
}

```
