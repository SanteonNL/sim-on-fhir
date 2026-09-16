Logical: SIMVerrichting
Id: SIMVerrichting
Title: "CLM SIMVerrichting"
Description: "Logisch model voor een verrichting binnen het Santeon Informatiemodel."

// nodig voor K312, later uitbouwen voor andere use cases
* VerrichtingID 0..1 Identifier "Identificatie van de verrichting"
* Patient 1..1 Reference "Patiënt waarop de verrichting betrekking heeft"
* VerrichtingType 0..1 CodeableConcept "Type verrichting"
* StartDatum 0..1 dateTime "Startdatum en -tijd van de verrichting"
* EindDatum 0..1 dateTime "Einddatum en -tijd van de verrichting"
// * UitvoerderSpecialisme 0..1 CodeableConcept "Specialisme van de uitvoerder"
* obeys validatie-verrichtingtype

// Optie?:
Invariant: validatie-verrichtingtype
Description: "Valideert VerrichtingType tegen de bij het gebruikte codesysteem behorende ValueSet."
Severity: #error
Expression: "(VerrichtingType.coding.where(system = 'NZa-URI').exists() implies VerrichtingType.memberOf('canonical-URL-NZa-ValueSet')) and (VerrichtingType.coding.where(system = 'DHD-URI').exists() implies VerrichtingType.memberOf('canonical-URL-DHD-ValueSet')) and (VerrichtingType.coding.where(system = 'http://snomed.info/sct').exists() implies VerrichtingType.memberOf('canonical-URL-SNOMED-ValueSet'))"

// Mappings van SIM CSV
Mapping: SIMVerrichtingFromSIMCSV
Id: SIMVerrichtingFromSIMCSV
Title: "SIM CSVs"
Source: SIMVerrichting
Target: "SIM CSVs"

* VerrichtingID -> "VerrichtingID"
* Patient -> "Identificatienummer"
// * VerrichtingType -> "VerrichtingTypeCodeNZaSysteem + VerrichtingTypeCodeNZa + VerrichtingTypeOmschrijvingNZa"
* VerrichtingType.coding.system -> "VerrichtingTypeCodeSysteem"
* VerrichtingType.coding.code -> "VerrichtingTypeCode"
* VerrichtingType.coding.display -> "VerrichtingTypeOmschrijving"
* StartDatum -> "VerrichtingStartDatum"
* EindDatum -> "VerrichtingEindDatum"
// * UitvoerderSpecialisme -> "UitvoerderSpecialisme"

// Mappings naar SIM FHIR
Mapping: SIMVerrichtingToFHIR
Id: SIMVerrichtingToFHIR
Title: "SIMVerrichting naar FHIR"
Source: SIMVerrichting
Target: "https://ig.santeon.nl/ibd/StructureDefinition/ProcedureSan"

* VerrichtingID -> "Procedure.identifier"
* Patient -> "Procedure.subject"
* VerrichtingType -> "Procedure.code"
* StartDatum -> "Procedure.performedPeriod.start"
* EindDatum -> "Procedure.performedPeriod.end"