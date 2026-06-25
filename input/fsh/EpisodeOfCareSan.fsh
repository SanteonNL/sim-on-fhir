Profile: EpisodeOfCareSan
Parent: EpisodeOfCare
Id: EpisodeOfCareSan
Title: "Santeon EpisodeOfCare (DBC)"

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
* status 1..1

// patient
* patient 1..1
* patient only Reference(PatientSan)

// period
* period 1..1
* period.start 1..1

// type
* type 1..1
* type.coding 1..*
* type.coding.system 1..1
* type.coding.code 1..1
* type.coding.display 0..1

// diagnose (diagnosis.codition verplicht in base) extensie gebasseerd op het SIM, dit moet naar condition gemodelleerd worden volgens FHIR
* diagnosis 1..*
// * diagnosis.extension contains SpecialismeDiagnoseCode named SpecialismeDiagnoseCode 1..1
* diagnosis.condition 1..1
* diagnosis.condition only Reference(ConditionSan)
* diagnosis.modifierExtension 0..0

// Laat alle andere elementen weg
* statusHistory 0..0
* managingOrganization 0..0
* referralRequest 0..0
* careManager 0..0
* team 0..0
* account 0..0

// mappings SIM -> FHIR
* ^mapping[+].identity = "sim"
* ^mapping[=].name = "Santeon Informatie Model"
* ^mapping[=].comment = "Mapping van EpisodeOfCare elementen naar SIM"

* identifier.value ^mapping[0].identity = "sim"
* identifier.value ^mapping[0].map = "DBC;SubtrajectNr"
* identifier.value ^mapping[0].comment = "uitleg/title"

* patient ^mapping[0].identity = "sim"
* patient ^mapping[0].map = "DBC;Identificatienummer"
* patient ^mapping[0].comment = "uitleg/title"

* status ^mapping[0].identity = "sim"
* status ^mapping[0].map = "DBC;DBCGeldig"
* status ^mapping[0].comment = "uitleg/title"

* period.start ^mapping[0].identity = "sim"
* period.start ^mapping[0].map = "DBC;DBCOpeningsDatum"
* period.start ^mapping[0].comment = "uitleg/title"

* type.coding.code ^mapping[0].identity = "sim"
* type.coding.code ^mapping[0].map = "DBC;ZorgTypeCode"
* type.coding.code ^mapping[0].comment = "uitleg/title"

* type.coding.system ^mapping[0].identity = "sim"
* type.coding.system ^mapping[0].map = "DBC;ZorgTypeCodeSysteem"
* type.coding.system ^mapping[0].comment = "uitleg/title"

* type.coding.display ^mapping[0].identity = "sim"
* type.coding.display ^mapping[0].map = "DBC;ZorgTypeOmschrijving"
* type.coding.display ^mapping[0].comment = "uitleg/title"

* diagnosis.condition ^mapping[0].identity = "sim"
* diagnosis.condition ^mapping[0].map = "DBC;SpecialismeDiagnoseCode"
* diagnosis.condition ^mapping[0].comment = "uitleg/title"

* diagnosis.extension.valueCodeableConcept.coding.code ^mapping[0].identity = "sim"
* diagnosis.extension.valueCodeableConcept.coding.code ^mapping[0].map = "DBC;SpecialismeDiagnoseCode"

* diagnosis.extension.valueCodeableConcept.coding.system ^mapping[0].identity = "sim"
* diagnosis.extension.valueCodeableConcept.coding.system ^mapping[0].map = "DBC;SpecialismeDiagnoseCodeSysteem"

* diagnosis.extension.valueCodeableConcept.coding.display ^mapping[0].identity = "sim"
* diagnosis.extension.valueCodeableConcept.coding.display ^mapping[0].map = "DBC;SpecialismeDiagnoseOmschrijving"