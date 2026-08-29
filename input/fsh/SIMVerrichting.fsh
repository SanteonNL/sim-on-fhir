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
* UitvoerderSpecialisme 0..1 CodeableConcept "Specialisme van de uitvoerder"


// Mappings van SIM CSV
Mapping: SIMVerrichtingFromSIMCSV
Id: SIMVerrichtingFromSIMCSV
Title: "Santeon Informatie Model (CSV)"
Source: SIMVerrichting
Target: "Santeon SIM CSV"

// * VerrichtingType -> "VerrichtingTypeCodeNZaSysteem + VerrichtingTypeCodeNZa + VerrichtingTypeOmschrijvingNZa"
* VerrichtingType.coding.system -> "VerrichtingTypeCode(NZa)Systeem"
* VerrichtingType.coding.code -> "VerrichtingTypeCode(NZa)"
* VerrichtingType.coding.display -> "VerrichtingTypeOmschrijving(NZa)"


// Mappings naar SIM FHIR
Mapping: SIMVerrichtingToFHIR
Id: SIMVerrichtingToFHIR
Title: "SIMVerrichting naar FHIR"
Source: SIMVerrichting
Target: "https://ig.santeon.nl/ibd/StructureDefinition/ProcedureSan"

* VerrichtingType -> "Procedure.code"
