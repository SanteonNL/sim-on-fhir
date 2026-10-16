# K3.1.2 Scopie - SIM on FHIR v0.1.0

## Binary: K3.1.2 Scopie

```

{
  "resourceType": "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/kpi-definition",
  "id": "k312-scopie",
  "title": "K3.1.2 Scopie",
  "description": "Aantal volwassen IBD-patiënten met een scopie-verrichting vanaf 2018.",
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
    }
  ]
}

```



## Resource Binary Content

application/fhir+json:

```
{snip}
```
