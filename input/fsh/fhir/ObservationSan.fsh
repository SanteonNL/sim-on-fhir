Profile: ObservationSan
Parent: Observation
Id: observation-san
Title: "Observation (SAN)"
Description: """
**De-identificatie:**
- identifier → hashed
- subject → hashed
- effectiveDateTime → dateshift
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

// status (in base verplicht)
* status 1..1

// code (in base verplicht)
* code 1..1
* code.coding 1..*
* code.coding.system 1..1
* code.coding.code 1..1
* code.coding.display 0..1

// subject
* subject 1..1
* subject only Reference(PatientSan)

// effective[x]
* effective[x] only dateTime

// value[x]: één uitslag, in één van de vier vormen die het SIM kent
// (UitslagWaarde, UitslagBereik, UitslagCode, UitslagDatumTijd)
* value[x] 0..1
* value[x] only Quantity or Range or CodeableConcept or dateTime

// Laat alle andere elementen weg
* basedOn 0..0
* partOf 0..0
* focus 0..0
* encounter 0..0
* issued 0..0
* performer 0..0
* dataAbsentReason 0..0
* interpretation 0..0
* note 0..0
* bodySite 0..0
* method 0..0
* specimen 0..0
* device 0..0
* referenceRange 0..0
* hasMember 0..0
* derivedFrom 0..0
* component 0..0

// mappings SIM -> FHIR
* ^mapping[+].identity = "sim"
* ^mapping[=].name = "Santeon Informatie Model"
* ^mapping[=].comment = "Mapping van Observation elementen naar SIM"

* identifier.value ^mapping[0].identity = "sim"
* identifier.value ^mapping[0].map = "AlgemeneMeting;MetingID"
* identifier.value ^mapping[0].comment = "hashed"

* subject ^mapping[0].identity = "sim"
* subject ^mapping[0].map = "AlgemeneMeting;Identificatienummer"
* subject ^mapping[0].comment = "hashed"

* code.coding.system ^mapping[0].identity = "sim"
* code.coding.system ^mapping[0].map = "AlgemeneMeting;MetingNaamCodeSysteem"

* code.coding.code ^mapping[0].identity = "sim"
* code.coding.code ^mapping[0].map = "AlgemeneMeting;MetingNaamCode"

* code.coding.display ^mapping[0].identity = "sim"
* code.coding.display ^mapping[0].map = "AlgemeneMeting;MetingNaamOmschrijving"


* effectiveDateTime ^mapping[0].identity = "sim"
* effectiveDateTime ^mapping[0].map = "AlgemeneMeting;MetingDatumTijd"
* effectiveDateTime ^mapping[0].comment = "dateshift"

// uitslag (value[x])
* valueQuantity.value ^mapping[0].identity = "sim"
* valueQuantity.value ^mapping[0].map = "AlgemeneMeting;UitslagWaarde"

* valueQuantity.system ^mapping[0].identity = "sim"
* valueQuantity.system ^mapping[0].map = "AlgemeneMeting;UitslagWaardeEenheidSysteem"

* valueQuantity.code ^mapping[0].identity = "sim"
* valueQuantity.code ^mapping[0].map = "AlgemeneMeting;UitslagWaardeEenheid"
* valueQuantity.code ^mapping[0].comment = "UCUM-code van de eenheid"

* valueQuantity.comparator ^mapping[0].identity = "sim"
* valueQuantity.comparator ^mapping[0].map = "AlgemeneMeting;UitslagWaardeOperator"

* valueRange.low.value ^mapping[0].identity = "sim"
* valueRange.low.value ^mapping[0].map = "AlgemeneMeting;ObservationValueRangeLow"

* valueRange.high.value ^mapping[0].identity = "sim"
* valueRange.high.value ^mapping[0].map = "AlgemeneMeting;ObservationValueRangeHigh"

* valueCodeableConcept.coding.system ^mapping[0].identity = "sim"
* valueCodeableConcept.coding.system ^mapping[0].map = "AlgemeneMeting;UitslagCodeSysteem"

* valueCodeableConcept.coding.code ^mapping[0].identity = "sim"
* valueCodeableConcept.coding.code ^mapping[0].map = "AlgemeneMeting;UitslagCode"

* valueCodeableConcept.coding.display ^mapping[0].identity = "sim"
* valueCodeableConcept.coding.display ^mapping[0].map = "AlgemeneMeting;UitslagCodeOmschrijving"

* valueDateTime ^mapping[0].identity = "sim"
* valueDateTime ^mapping[0].map = "AlgemeneMeting;UitslagDatumTijd"
* valueDateTime ^mapping[0].comment = "dateshift"
