<!--
In deze implementation guide zijn de volgende indicatoren tot nu toe vastgelegd voor de IBD use case:


### K3.6.3 f_Doorlooptijd_meting_tot_verrichting

Formaliseert de IBD kwaliteitsindicator K3.6.3: *"Percentage scopieën waarbij tot 90 dagen voorafgaand aan de scopie een calprotectine meting is gedaan"*.

Zie de [IndicatorK363-calprotectine](StructureDefinition-ibd-calprotectine-meting.html) voor meer details. // See the [K3.6.3 Measure](Measure-SanteonIBDMeasureK363.html) definition, beter? (uittesten)

Voor deze indicator zijn op basis van de R functie f_Zorgactiviteit de volgende mappings gemaakt:

| Datamodel-veld     | FHIR resource   | Element                          |
|----------------------|-----------------|----------------------------------|
| `Identificatienummer`| `Observation`   | `subject` → Patient              |
| `MetingNaamCode` (38445-3) | `Observation` | `code` (LOINC 38445-3)        |
| `MetingDatumTijd`    | `Observation`   | `effectiveDateTime`              |
| window 90 dagen      | `Observation`   | `effectiveDateTime` ≤ scopiedatum, ≥ scopiedatum - 90d |
| koppeling aan scopie | `Observation`   | `partOf` → IBDScopie             |


Componenten en definities:

| Component | Definitie |
|-----------|----------|
| **Initial Population** | Patiënten met ≥1 scopie (NZa ∈ {034620, 034686, 034690, 035582}) |
| **Denominator** | Alle scopie-verrichtingen (SanteonProcedure) |
| **Numerator** | Scopies met calprotectine meting (LOINC 38445-3) ≤90 dagen voorafgaand (SanteonObservation) |
| **Scoring** | Proportion (percentage) — hoger = beter |


Berekeningslogica:

```
Voor elke scopie (Procedure) in denominator:
  Zoek Observation waar:
    - code = LOINC 38445-3
    - subject = zelfde patiënt
    - effectiveDateTime ∈ [scopie.start - 90d, scopie.start]
  Als gevonden → telt mee in numerator

Indicator = numerator / denominator × 100%
```

---
-->
### K3.1.2 f_Zorgactiviteit

De initial population kan optioneel worden verfijnd via indicator K3.1.2 (`f_Zorgactiviteit`), die de scopie-populatie filtert op DBC-context (Contact, DBC, Fasering). 

Zie de [IndicatorK312-scopie](StructureDefinition-ibd-scopie.html) voor meer details.

Voor deze indicator zijn op basis van de R functie f_Zorgactiviteit de volgende mappings gemaakt:

| Datamodel-veld                  | FHIR resource  | Element                          |
|-----------------------------------|----------------|----------------------------------|
| `Identificatienummer`             | `Procedure`    | `subject` → Patient              |
| `VerrichtingTypeCodeNZa`          | `Procedure`    | `code` (IBDScopieVerrichtingVS)  |
| `VerrichtingStartDatum`           | `Procedure`    | `performedDateTime`              |
| `Uitvoerder_Specialisme`          | `Procedure`    | `performer.actor` → PractitionerRole |
| `ContactID` / `ContactTypeCode`   | `Encounter`    | `class` (SS/IMP)                 |
| `BeginDatumTijd` / `EindDatumTijd`| `Encounter`    | `period.start` / `period.end`    |
| `EncounterPriorityCodingCode`     | `Encounter`    | `priority` (R/EM)                |
| `DiagnoseCode`                    | `Condition`    | `code` (IBDDiagnoseVS)           |
| `SubtrajectNr`                    | `EpisodeOfCare`| `identifier.value`               |

---
---
### Indicator K3.1.2 - % patiënten dat een scopie krijgt

Dit overzicht toont de opbouw van de kwaliteitsindicator en de onderliggende FHIR-artefacten.

*   **Indicator-definitie:** [Measure/MeasureK312](Measure-MeasureK312.html)
    *   **Logica-bibliotheek:** [Library/K312Criteria](Library-K312Criteria.html) (CQL)
        *   **Startpopulatie (IBDCohort):** Filter op basis van DBC-registraties.
            *   *HeeftIbdCarePlan (CarePlan ZBJ_IBD >= 2018) — Voor nu buiten scope*
            *   **HeeftDbcMdl:** Specialisme `0318` + Diagnose `601`/`602` + Zorgtype `11`/`21`
            *   **HeeftDbcInterne:** Specialisme `0313` + Diagnose `922`/`923` + Zorgtype `11`/`21`
        *   **Noemer (Denominator):** Gelijk aan `IBDCohort`
        *   **Teller (Numerator):** `IBDCohort` + `HeeftScopie`

#### Benodigde Waardelijsten (ValueSets)
*   [ValueSet/vs-scopie-nza](ValueSet-vs-scopie-nza.html) — NZa-verrichtingscodes voor scopie (034620, 034686, 034690, 035582)
*   [ValueSet/vs-zorgtype-cohort](ValueSet-vs-zorgtype-cohort.html) — DBC-zorgtypes voor reguliere zorg (11, 21)
*   [ValueSet/vs-ibd-diagnose-mdl](ValueSet-vs-ibd-diagnose-mdl.html) — IBD diagnosecodes Maag-Darm-Leverziekten (601, 602)
*   [ValueSet/vs-ibd-diagnose-interne](ValueSet-vs-ibd-diagnose-interne.html) — IBD diagnosecodes Interne Geneeskunde (922, 923)

#### Gebruikte Basisprofielen
*   [StructureDefinition/nl-dbc](StructureDefinition-nl-dbc.html) — Generiek profiel op `EpisodeOfCare` voor DBC-registraties.
*   [StructureDefinition/nl-verrichting](StructureDefinition-nl-verrichting.html) — Generiek profiel op `Procedure` voor NZa-verrichtingen.
