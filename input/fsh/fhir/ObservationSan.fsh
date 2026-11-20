Profile: ObservationSan
Parent: Observation
Id: observation-san
Title: "Observation (SAN)"
Description: "Generiek Santeon-profiel voor metingen (SIMAlgemeneMeting), gebaseerd op FHIR R4 Observation (er is geen Nictiz GBB-profiel; het HL7 Europe-profiel is specifiek voor testuitslagen). Datasetprofielen sluiten de elementen die zij niet gebruiken."

// Opgenomen
* identifier 1..1 MS
* identifier.value 1..1
* status 1..1 MS
* code 1..1 MS
* code from Santeon_MetingNaamCodelijst (required)
* code.coding 1..*
* code.coding.system 1..1
* code.coding.code 1..1
* subject 1..1 MS
* subject only Reference(PatientSan)
* effective[x] 0..1 MS
* effective[x] only dateTime
* value[x] 0..1 MS
* value[x] only Quantity or Range or CodeableConcept or dateTime
* method 0..1 MS

// Uitgesloten
* implicitRules 0..0
* text 0..0
* contained 0..0
* modifierExtension 0..0


Mapping: ObservationSanFromSIMCSV
Id: sim
Title: "Santeon Informatiemodel (SIM CSV)"
Description: "Informatieve mapping van FHIR Observation naar SIM CSV-bronvelden."
Source: ObservationSan
Target: "urn:santeon:sim:csv"

* identifier.system -> "AlgemeneMeting;MetingID" "Systeemdeel van de samengestelde identifier."
* identifier.value -> "AlgemeneMeting;MetingID" "Waardedeel van de samengestelde identifier; hash."
* subject -> "AlgemeneMeting;Identificatienummer" "Patient-reference op basis van patiëntidentifier."
* code.coding.system -> "AlgemeneMeting;MetingNaamCodeSysteem"
* code.coding.code -> "AlgemeneMeting;MetingNaamCode"
* code.coding.display -> "AlgemeneMeting;MetingNaamOmschrijving"
* effectiveDateTime -> "AlgemeneMeting;MetingDatumTijd"
* valueQuantity.value -> "AlgemeneMeting;UitslagWaarde"
* valueQuantity.system -> "AlgemeneMeting;UitslagWaardeEenheidSysteem"
* valueQuantity.code -> "AlgemeneMeting;UitslagWaardeEenheid"
* valueQuantity.comparator -> "AlgemeneMeting;UitslagWaardeOperator"
* valueRange.low.value -> "AlgemeneMeting;ObservationValueRangeLow"
* valueRange.low.unit -> "AlgemeneMeting;ObservationValueRangeLowUnit"
* valueRange.low.system -> "AlgemeneMeting;ObservationValueRangeLowSystem"
* valueRange.low.code -> "AlgemeneMeting;ObservationValueRangeLowCode"
* valueRange.high.value -> "AlgemeneMeting;ObservationValueRangeHigh"
* valueRange.high.unit -> "AlgemeneMeting;ObservationValueRangeHighUnit"
* valueRange.high.system -> "AlgemeneMeting;ObservationValueRangeHighSystem"
* valueRange.high.code -> "AlgemeneMeting;ObservationValueRangeHighCode"
* valueCodeableConcept.coding.system -> "AlgemeneMeting;UitslagCodeSysteem"
* valueCodeableConcept.coding.code -> "AlgemeneMeting;UitslagCode"
* valueCodeableConcept.coding.display -> "AlgemeneMeting;UitslagCodeOmschrijving"
* valueDateTime -> "AlgemeneMeting;UitslagDatumTijd"
* method.coding.system -> "AlgemeneMeting;MeetmethodeCodeSysteem"
* method.coding.code -> "AlgemeneMeting;MeetmethodeCode"
* method.coding.display -> "AlgemeneMeting;MeetmethodeOmschrijving"
