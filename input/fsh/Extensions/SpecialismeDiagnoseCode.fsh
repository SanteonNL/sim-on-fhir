// dit moet anders gemodelleerd worden en gaan vallen onder condition

Extension: SpecialismeDiagnoseCode
Id: specialisme-diagnose-code
Title: "SpecialismeDiagnoseCode"
Description: "(Tijdelijke) Extensie voor SpecialismeDiagnoseCode. Deze informatie dient in een toekomstige FHIR-conforme modellering te worden ondergebracht in Condition.code."

* value[x] only CodeableConcept
* valueCodeableConcept from LocalSpecialismeDiagnoseCodes (required)

* valueCodeableConcept.coding 1..*
* valueCodeableConcept.coding.system 1..1
* valueCodeableConcept.coding.code 1..1
* valueCodeableConcept.coding.display 0..1

// mappings SIM -> FHIR
* ^mapping[+].identity = "sim"
* ^mapping[=].name = "Santeon Informatie Model"
* ^mapping[=].comment = "Mapping van Extensie elementen naar SIM"

* valueCodeableConcept.coding.code ^mapping[0].identity = "sim"
* valueCodeableConcept.coding.code ^mapping[0].map = "DBC;SpecialismeDiagnoseCode"
* valueCodeableConcept.coding.code ^mapping[0].comment = "uitleg/title"


* valueCodeableConcept.coding.system ^mapping[0].identity = "sim"
* valueCodeableConcept.coding.system ^mapping[0].map = "DBC;SpecialismeDiagnoseCodeSysteem"
* valueCodeableConcept.coding.system ^mapping[0].comment = "uitleg/title"

* valueCodeableConcept.coding.display ^mapping[0].identity = "sim"
* valueCodeableConcept.coding.display ^mapping[0].map = "DBC;SpecialismeDiagnoseOmschrijving"
* valueCodeableConcept.coding.display ^mapping[0].comment = "uitleg/title"