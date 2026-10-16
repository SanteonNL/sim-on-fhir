# Alcoholgebruik - SIM on FHIR v0.1.0

## ObservationDefinition: Alcoholgebruik 

**category**: Social History

**code**: Finding relating to alcohol drinking behavior (finding)

**permittedDataType**: CodeableConcept

**preferredReportName**: Alcoholgebruik

**validCodedValueSet**: [http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.40.2.7.3.2](http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.40.2.7.3.2)



## Resource Content

```json
{
  "resourceType" : "ObservationDefinition",
  "id" : "od-alcohol-use",
  "category" : [{
    "coding" : [{
      "system" : "http://terminology.hl7.org/CodeSystem/observation-category",
      "code" : "social-history"
    }]
  }],
  "code" : {
    "coding" : [{
      "system" : "http://snomed.info/sct",
      "code" : "228273003",
      "display" : "Finding relating to alcohol drinking behavior (finding)"
    }]
  },
  "permittedDataType" : ["CodeableConcept"],
  "preferredReportName" : "Alcoholgebruik",
  "validCodedValueSet" : {
    "reference" : "http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.40.2.7.3.2"
  }
}

```
