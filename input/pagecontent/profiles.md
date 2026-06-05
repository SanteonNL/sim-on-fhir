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
