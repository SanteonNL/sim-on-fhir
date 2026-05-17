# Profiles

## Observation (AlgemeneMeting)

See the [SanteonObservation](StructureDefinition-santeon-observation.html) profile for details.

Mapped to ZIB AlgemeneMeting-v3.0(2019NL). Used for general measurements including calprotectine (LOINC `38445-3`) in the IBD use case (indicator K3.6.3: % scopieën waarbij tot 90 dagen voorafgaand aan de scopie een calprotectine meting is gedaan).

| ZIB-veld | FHIR path | Deident |
|----------|-----------|----------|
| Identificatienummer | `Observation.subject` → `Patient.identifier:bsn` | hash |
| MetingDatumTijd | `Observation.effectiveDateTime` | dateshift |
| MetingNaamCode | `Observation.code.coding.code` | — |

## Procedure (Verrichting)

See the [SanteonProcedure](StructureDefinition-santeon-procedure.html) profile for details.

Mapped to ZIB Verrichting-v5.1(2019NL). Used for procedures including scopie (NZa codes 034620, 034686, 034690, 035582) in the IBD use case (indicator K3.6.3).

| ZIB-veld | FHIR path | Deident |
|----------|-----------|----------|
| Identificatienummer | `Procedure.subject` → `Patient.identifier:bsn` | hash |
| VerrichtingStartDatum | `Procedure.performedPeriod.start` | dateshift |
| VerrichtingTypeCodeNZa | `Procedure.code.coding[nza].code` | — |
| VerrichtingTypeCodeSnomedCT | `Procedure.code.coding[snomed].code` | — |
| VerrichtingTypeCodeDHD | `Procedure.code.coding[dhd].code` | — |
| VerrichtingAantal | *(geen standaard FHIR mapping)* | — |
| Uitvoerder_Specialisme | `Procedure.performer.actor` → `Practitioner.qualification` | — |
| Aanvrager_Specialisme | `Procedure.basedOn` → `ServiceRequest.requester` → `Practitioner.qualification` | — |

## Measure (Kwaliteitsindicator K3.6.3)

See the [K3.6.3 Measure](Measure-SanteonIBDMeasureK363.html) definition.

Formaliseert de IBD kwaliteitsindicator K3.6.3: *"Percentage scopieën waarbij tot 90 dagen voorafgaand aan de scopie een calprotectine meting is gedaan"*.

Vervangt de R-functie `f_Doorlooptijd_meting_tot_verrichting` uit HipsETL.

| Component | Definitie |
|-----------|----------|
| **Initial Population** | Patiënten met ≥1 scopie (NZa ∈ {034620, 034686, 034690, 035582}) |
| **Denominator** | Alle scopie-verrichtingen (SanteonProcedure) |
| **Numerator** | Scopies met calprotectine meting (LOINC 38445-3) ≤90 dagen voorafgaand (SanteonObservation) |
| **Scoring** | Proportion (percentage) — hoger = beter |

### Berekeningslogica

```
Voor elke scopie (Procedure) in denominator:
  Zoek Observation waar:
    - code = LOINC 38445-3
    - subject = zelfde patiënt
    - effectiveDateTime ∈ [scopie.start - 90d, scopie.start]
  Als gevonden → telt mee in numerator

Indicator = numerator / denominator × 100%
```

### K3.1.2 Afhankelijkheid

De initial population kan optioneel worden verfijnd via indicator K3.1.2 (`f_Zorgactiviteit`), die de scopie-populatie filtert op DBC-context (Contact, DBC, Fasering). Dit is **niet vereist** voor de kernberekening van K3.6.3 — de indicator werkt standalone op basis van Procedure + Observation.

## MeasureReport (Indicatorrapportage)

See the [SanteonMeasureReport](StructureDefinition-santeon-measurereport.html) profile for details.

Rapporteert de berekende waarden van een Measure over een rapportageperiode. Bevat numerator, denominator counts en de resulterende score.

### Twee rapportage-typen

| Type | Doel | subject | evaluatedResource |
|------|------|---------|-------------------|
| **summary** | Geaggregeerde score over alle patiënten | — | — |
| **individual** | Score voor één patiënt | Patient reference | Verwijzingen naar concrete SanteonObservation + SanteonProcedure |

### Relatie tussen resources

```
MeasureReport
  ├── measure → Measure K3.6.3 (indicator definitie)
  ├── subject → Patient (alleen bij type=individual)
  └── evaluatedResource (alleen bij type=individual)
        ├── → SanteonObservation (calprotectine meting)
        └── → SanteonProcedure (scopie verrichting)
```
