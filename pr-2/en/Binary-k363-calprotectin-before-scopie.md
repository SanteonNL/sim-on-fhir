# K3.6.3 Calprotectine voor scopie - SIM on FHIR v0.1.0

## Binary: K3.6.3 Calprotectine voor scopie

```

{
  "resourceType": "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/kpi-definition",
  "id": "k363-calprotectin-before-scopie",
  "title": "K3.6.3 Calprotectine voor scopie",
  "description": "Percentage scopieën waarbij in de 90 dagen voorafgaand aan de scopie ten minste één fecaal calprotectinemeting is uitgevoerd.",
  "version": "0.1.0",
  "criteria": [
    {
      "resource": "Procedure",
      "param": [
        {
          "name": "code:in",
          "value": "{vs:local-verrichting-codes-nza}"
        },
        {
          "name": "date",
          "value": "ge2018-01-01"
        }
      ],
      "element": [
        "code",
        "performed[x]",
        "subject"
      ]
    },
    {
      "resource": "Observation",
      "param": [
        {
          "name": "code",
          "value": "{od:od-calprotectin.code}"
        },
        {
          "name": "date",
          "value": "ge2018-01-01"
        }
      ],
      "element": [
        "code",
        "value[x]",
        "effective[x]",
        "subject"
      ]
    }
  ],
  "postFilter": [
    {
      "description": "A calprotectin measurement counts only when it falls within the 90 days up to and including the start of a scopie of the same patient. A date relation between two resources cannot be expressed in a FHIR search.",
      "expression": "Observation.effective >= Procedure.performed.start - 90 days and Observation.effective <= Procedure.performed.start and Observation.subject = Procedure.subject"
    }
  ]
}

```



## Resource Binary Content

application/fhir+json:

```
{snip}
```
