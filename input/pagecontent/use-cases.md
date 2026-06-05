- 1. **IBD use case**
    - Zie de omschrijving van het [Cohort IBD](use-cases.html#cohort-ibd) op deze pagina.
    - En de [Indicator K363](indicatoren.html#k363--f_doorlooptijd_meting_tot_verrichting) op de subpagina [Indicatoren](indicatoren.html).

### Cohort IBD

Zie het [Santeon IBD Cohort](StructureDefinition-santeon-ibd-cohort.html) voor details.

Het cohort bevat ... [omschrijving]

Op basis van de datasource_queries (HIX) zijn de volgende mappings gemaakt:

| SQL-veld            | FHIR resource      | Element                              |
|---------------------|--------------------|--------------------------------------|
| `PATIENTNR`         | `Patient`          | `identifier[patientnummer].value`    |
| `GEBDAT`            | `Patient`          | `birthDate`                          |
| `BEGINDAT`          | `EpisodeOfCare`    | `period.start`                       |
| `SPECIALISM` (0313/0318) | `EpisodeOfCare` | `extension[specialisme].valueCoding` |
| `HOOFDDIAG`         | `EpisodeOfCare`    | `extension[diagnoseCode].valueCoding`|
| `ZORGTYPE` (11/21)  | `EpisodeOfCare`    | `extension[zorgtype].valueCoding`    |

---
---

## Relaties tussen resources?

```
IBDPatient (1)
    │
    ├──── IBDEpisodeOfCare (DOT/subtraject)
    │         └── extensies: specialisme, diagnoseCode, zorgtype
    │
    ├──── IBDContact (Encounter)
    │         └── episodeOfCare → IBDEpisodeOfCare
    │
    ├──── IBDCondition (Diagnose)
    │         └── encounter → IBDContact
    │
    ├──── IBDScopie (Procedure / K3.1.2)
    │         ├── encounter → IBDContact
    │         └── reasonReference → IBDCondition
    │
    └──── IBDCalprotectineMeting (Observation / K3.6.3)
              ├── partOf → IBDScopie
              └── effectiveDateTime within [scopiedatum-90d, scopiedatum]
```
