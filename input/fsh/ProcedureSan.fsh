Profile: ProcedureSan
Parent: Procedure
Id: ProcedureSan
Title: "Santeon Procedure (Verrichting)"
Description: """
**De-identificatie:**
- identifier → hashed
- subject → hashed
"""

// Meta overhead
* meta 0..0
* implicitRules 0..0
* language 0..0
* text 0..0
* contained 0..0
* extension 0..0
* modifierExtension 0..0

// identifier
* identifier 1..1
* identifier.use 0..0
* identifier.value 1..1

// status verplicht in base

// code
* code 1..1
* code.coding 1..*
* code.coding.system 1..1
* code.coding.code 1..1
* code.coding.display 0..1

// subject
* subject 1..1
* subject only Reference(PatientSan)

// performed[x]
* performed[x] only Period
* performedPeriod.start 1..1
* performedPeriod.end 0..1

// Laat alle andere elementen weg
* instantiatesCanonical 0..0
* instantiatesUri 0..0
* partOf 0..0
* statusReason 0..0
* category 0..0
* encounter 0..0
* recorder 0..0
* asserter 0..0
* location 0..0
* reasonCode 0..0
* reasonReference 0..0
* bodySite 0..0
* outcome 0..0
* report 0..0
* complication 0..0
* complicationDetail 0..0
* followUp 0..0
* note 0..0
* focalDevice 0..0
* usedReference 0..0
* usedCode 0..0


// mappings SIM -> FHIR
* ^mapping[+].identity = "sim"
* ^mapping[=].name = "Santeon Informatie Model"
* ^mapping[=].comment = "Mapping van Procedure elementen naar SIM"

* identifier.value ^mapping[0].identity = "sim"
* identifier.value ^mapping[0].map = "Verrichting;VerrichtingID"
* identifier.value ^mapping[0].comment = "hashed"

* subject ^mapping[0].identity = "sim"
* subject ^mapping[0].map = "Verrichting;Identificatienummer"
* subject ^mapping[0].comment = "hashed"

* code.coding.code ^mapping[0].identity = "sim"
* code.coding.code ^mapping[0].map = "Verrichting;VerrichtingTypeCode"

* code.coding.system ^mapping[0].identity = "sim"
* code.coding.system ^mapping[0].map = "Verrichting;VerrichtingTypeCodeSysteem"

* code.coding.display ^mapping[0].identity = "sim"
* code.coding.display ^mapping[0].map = "Verrichting;VerrichtingTypeOmschrijving"

* performedPeriod.start ^mapping[0].identity = "sim"
* performedPeriod.start ^mapping[0].map = "Verrichting;VerrichtingStartDatum"
* performedPeriod.start ^mapping[0].comment = "dateshift"

* performedPeriod.end ^mapping[0].identity = "sim"
* performedPeriod.end ^mapping[0].map = "Verrichting;VerrichtingEindDatum"
* performedPeriod.end ^mapping[0].comment = "dateshift"