Profile: ConditionSan
Parent: Condition
Id: ConditionSan
Title: "Santeon Condition (Probleem)"
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

// code
* code 1..1
* code.coding 1..*
* code.coding.system 1..1
* code.coding.code 1..1
* code.coding.display 0..1
// * code from LocalSpecialismeDiagnoseCodes

// subject (in base verplicht)
* subject 1..1
* subject only Reference(PatientSan)

// Laat alle andere elementen weg
* clinicalStatus 0..0
* verificationStatus 0..0
* category 0..0
* severity 0..0
* bodySite 0..0
* encounter 0..0
* onset[x] 0..0
* abatement[x] 0..0
* recordedDate 0..0
* recorder 0..0
* asserter 0..0
* stage 0..0
* evidence 0..0
* note 0..0

// mappings SIM -> FHIR
* ^mapping[+].identity = "sim"
* ^mapping[=].name = "Santeon Informatie Model"
* ^mapping[=].comment = "Mapping van Condition elementen naar SIM"

* identifier.value ^mapping[0].identity = "sim"
* identifier.value ^mapping[0].map = "Probleem;ProbleemID"
* identifier.value ^mapping[0].comment = "hashed"

* code.coding.code ^mapping[0].identity = "sim"
* code.coding.code ^mapping[0].map = "DBC;SpecialismeDiagnoseCode"

* code.coding.system ^mapping[0].identity = "sim"
* code.coding.system ^mapping[0].map = "DBC;SpecialismeDiagnoseCodeSysteem"

* code.coding.display ^mapping[0].identity = "sim"
* code.coding.display ^mapping[0].map = "DBC;SpecialismeDiagnoseOmschrijving"

* subject ^mapping[0].identity = "sim"
* subject ^mapping[0].map = "Probleem;Identificatienummer"
* subject ^mapping[0].comment = "hashed"
