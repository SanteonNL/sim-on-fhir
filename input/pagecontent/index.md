# Santeon IBD Implementation Guide

This implementation guide describes the Santeon FHIR profiles for IBD (Inflammatory Bowel Disease) quality indicators within the Santeon hospital network.

## Scope

- **SIM Profiles**: Observation (AlgemeneMeting), Procedure (Verrichting)
- **Use cases**: IBD,
- **Indicatoren**: *Indicator K3.6.3* Percentage scopieën waarbij tot 90 dagen voorafgaand aan de scopie een calprotectine meting is gedaan & MeasureReport.
- **Source**: HIPS data model (ZIB-based)



## Structuur van de IG

```
ig-ibd/
├── sushi-config.yaml          ← IG-metadata, versie, dependencies
└── input/fsh/
    ├── 01-cohort.fsh          ← IBDPatient, IBDEpisodeOfCare + extensies + valuesets
    ├── 02-K312-scopie.fsh     ← IBDScopie (Procedure), IBDContact, IBDCondition
    ├── 03-K363-calprotectine.fsh ← IBDCalprotectineMeting, Measure, MeasureReport
    └── 04-examples.fsh        ← Testinstanties (validatie + documentatie)
```

---
