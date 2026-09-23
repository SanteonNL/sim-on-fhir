# Santeon Standard De-identification Ruleset - Santeon CarePlan Implementation Guide v0.1.0

## Binary: Santeon Standard De-identification Ruleset

```

{
  "resourceType": "https://ig.santeon.nl/careplan/StructureDefinition/deidentification-ruleset",
  "id": "santeon-default",
  "url": "https://ig.santeon.nl/sim-on-fhir/DeidentificationRuleset/santeon-default",
  "version": "0.1.0",
  "rule": [
    {
      "path": "Patient.id",
      "action": "hash",
      "algorithm": "hmac-sha256",
      "propagateTo": [
        "*.subject",
        "*.patient"
      ]
    },
    {
      "path": "Patient.identifier",
      "action": "hash",
      "algorithm": "hmac-sha256"
    },
    {
      "path": "**.ofType(date)",
      "action": "shift",
      "maxDays": 15
    },
    {
      "path": "Patient.birthDate",
      "action": "first-of-month"
    },
    {
      "path": "Patient.birthDate",
      "action": "clamp-age",
      "minAge": 18,
      "maxAge": 85
    }
  ]
}

```



## Resource Binary Content

application/fhir+json:

```
{snip}
```
