# IBD dataset - SIM on FHIR v0.1.0

## Binary: IBD dataset

```

{
  "resourceType": "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/dataset-definition",
  "id": "ibd-dataset",
  "title": "IBD",
  "description": "Gegevensset voor volwassen patiënten met inflammatoire darmziekten (IBD): scopieën en calprotectinemetingen, plus alcoholgebruik.",
  "version": "0.1.0",
  "cohort": {
    "description": "Alle patiënten van 18 jaar of ouder op het moment van aanleveren met een actief, regulier DBC-zorgtraject\n(zorgtype 11 of 21) dat op of na 01-01-2018 is geopend en waarvan de specialisme-diagnose bij IBD hoort.\nDe startdatum 01-01-2018 is bewust niet dynamisch.",
    "criteria": [
      {
        "resource": "Patient",
        "param": [
          {
            "name": "birthdate",
            "value": "le{today-18y}"
          }
        ]
      },
      {
        "resource": "EpisodeOfCare",
        "param": [
          {
            "name": "type:in",
            "value": "{vs:local-zorgtype-codes}"
          },
          {
            "name": "status",
            "value": "active"
          },
          {
            "name": "date",
            "value": "ge2018-01-01"
          },
          {
            "name": "diagnosis:Condition.code:in",
            "value": "{vs:local-specialisme-diagnose-codes}"
          }
        ]
      }
    ]
  },
  "include": [
    {
      "resource": "Patient",
      "param": [
        {
          "name": "gender",
          "value": "male,female"
        }
      ],
      "element": [
        "gender",
        "birthDate"
      ]
    }
  ],
  "kpi": [
    "k312-scopie",
    "k363-calprotectin-before-scopie",
    "alcohol-status-recorded"
  ]
}

```



## Resource Binary Content

application/fhir+json:

```
{snip}
```
