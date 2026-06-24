# Profiles

## EpisodeOfCare Santeon (DBC)

See the [EpisodeOfCareSan](StructureDefinition-EpisodeOfCareSan.html) profile for details.

| SIM-veld | FHIR path | Deident |
|----------|-----------|----------|
| DBCOpeningsDatum | `EpisodeOfCare.period.start` | dateshift |
| etc. | |

## Patient Santeon

See the [PatientSan](StructureDefinition-PatientSan.html) profile for details.

| SIM-veld | FHIR path | Deident |
|----------|-----------|----------|
| Geboortedatum | `Patient.birthDate` | dateshift |
| etc. | |

## Procedure Santeon (Verrichting)

See the [ProcedureSan](StructureDefinition-ProcedureSan.html) profile for details.

| SIM-veld | FHIR path | Deident |
|----------|-----------|----------|
| VerrichtingTypeCode | `Procedure.code.coding.code` | — |
| etc. | |

---
# Archief
- See the [SanteonObservation](StructureDefinition-santeon-observation.html) profile for details.
- See the [SanteonProcedure](StructureDefinition-santeon-procedure.html) profile for details.

<!-- voorbeeld:

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
-->





