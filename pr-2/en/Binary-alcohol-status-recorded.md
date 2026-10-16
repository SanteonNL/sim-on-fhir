# Alcoholgebruik vastgelegd - SIM on FHIR v0.1.0

## Binary: Alcoholgebruik vastgelegd

```

{
  "resourceType": "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/kpi-definition",
  "id": "alcohol-status-recorded",
  "title": "Alcoholgebruik vastgelegd",
  "description": "Is de alcoholgebruikstatus vastgelegd met een gecodeerde uitslag?",
  "version": "0.1.0",
  "criteria": [
    {
      "resource": "Observation",
      "param": [
        {
          "name": "code",
          "value": "{od:od-alcohol-use.code}"
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
  ]
}

```



## Resource Binary Content

application/fhir+json:

```
{snip}
```
