# Calprotectine (feces) - SIM on FHIR v0.1.0

## ObservationDefinition: Calprotectine (feces) 

**category**: Laboratory

**code**: Calprotectin [Mass/volume] in Stool

**permittedDataType**: Quantity

**preferredReportName**: Calprotectine (feces)

### QuantitativeDetails

| | |
| :--- | :--- |
| - | **Unit** |
| * | microgram per gram |



## Resource Content

```json
{
  "resourceType" : "ObservationDefinition",
  "id" : "od-calprotectin",
  "category" : [{
    "coding" : [{
      "system" : "http://terminology.hl7.org/CodeSystem/observation-category",
      "code" : "laboratory"
    }]
  }],
  "code" : {
    "coding" : [{
      "system" : "http://loinc.org",
      "code" : "38445-3",
      "display" : "Calprotectin [Mass/volume] in Stool"
    }]
  },
  "permittedDataType" : ["Quantity"],
  "preferredReportName" : "Calprotectine (feces)",
  "quantitativeDetails" : {
    "unit" : {
      "coding" : [{
        "system" : "http://unitsofmeasure.org",
        "code" : "ug/g",
        "display" : "microgram per gram"
      }]
    }
  }
}

```
