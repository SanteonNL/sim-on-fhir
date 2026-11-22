Profile: ConditionSan
Parent: http://ig.nictiz.nl/gbb/StructureDefinition/nl-core-Condition
Id: condition-san
Title: "Condition (SAN)"
Description: "Generiek Santeon-profiel voor aandoeningen en diagnoses, gebaseerd op Nictiz nl-core-Condition. Datasetprofielen sluiten de elementen die zij niet gebruiken."

// Opgenomen
* identifier 0..* MS
* identifier.value 1..1
* code 1..1 MS
* code.coding 1..*
* code.coding.system 1..1
* code.coding.code 1..1
* subject 1..1 MS
* subject only Reference(PatientSan)

// Uitgesloten
* implicitRules 0..0
* text 0..0
* contained 0..0
* modifierExtension 0..0


Mapping: ConditionSanFromDBC
Id: ConditionSanFromDBC
Title: "ConditionSan vanuit SIM DBC"
Source: ConditionSan
Target: "urn:santeon:sim:csv"

* subject -> "DBC;Identificatienummer" "Patient-reference op basis van identificatienummer."
* code.coding.system -> "DBC;SpecialismeDiagnoseSysteem" "DBC-diagnosecodes per specialisme: urn:oid:2.16.840.1.113883.2.4.3.27.15.4.<specialisme zonder voorloopnullen>, bijv. 0313 -> ...27.15.4.313."
* code.coding.code -> "DBC;SpecialismeDiagnoseCode" "Alleen de diagnosecode (bijv. 922), zonder specialismeprefix."
* code.coding.display -> "DBC;SpecialismeDiagnoseOmschrijving"
