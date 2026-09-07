Logical: SIMVerrichting
Id: SIMVerrichting
Title: "SIM Verrichting"
Description: "Logisch model voor een verrichting binnen het Santeon Informatiemodel."

// nodig voor K312, later uitbouwen voor andere use cases
* VerrichtingID 0..1 Identifier "Identificatie van de verrichting"
* Patient 1..1 Reference "Patiënt waarop de verrichting betrekking heeft"
* VerrichtingType 0..1 CodeableConcept "Type verrichting"
* StartDatum 0..1 dateTime "Startdatum en -tijd van de verrichting"
* EindDatum 0..1 dateTime "Einddatum en -tijd van de verrichting"
// * UitvoerderSpecialisme 0..1 CodeableConcept "Specialisme van de uitvoerder"

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