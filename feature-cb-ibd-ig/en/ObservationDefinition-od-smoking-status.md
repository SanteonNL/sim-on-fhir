# Rookstatus - SIM on FHIR v0.1.0

## ObservationDefinition: Rookstatus 

**category**: Social History

**code**: Tobacco smoking status

**permittedDataType**: CodeableConcept

**preferredReportName**: Rookstatus

**validCodedValueSet**: [http://snomed.info/sct?fhir_vs=ecl/%3C%3C%2077176002%20OR%20%3C%3C%208392000%20OR%20%3C%3C%208517006](http://snomed.info/sct?fhir_vs=ecl/%3C%3C%2077176002%20OR%20%3C%3C%208392000%20OR%20%3C%3C%208517006)



## Resource Content

```json
{
  "resourceType" : "ObservationDefinition",
  "id" : "od-smoking-status",
  "category" : [{
    "coding" : [{
      "system" : "http://terminology.hl7.org/CodeSystem/observation-category",
      "code" : "social-history"
    }]
  }],
  "code" : {
    "coding" : [{
      "system" : "http://loinc.org",
      "code" : "72166-2",
      "display" : "Tobacco smoking status"
    }]
  },
  "permittedDataType" : ["CodeableConcept"],
  "preferredReportName" : "Rookstatus",
  "validCodedValueSet" : {
    "reference" : "http://snomed.info/sct?fhir_vs=ecl/%3C%3C%2077176002%20OR%20%3C%3C%208392000%20OR%20%3C%3C%208517006"
  }
}

```
