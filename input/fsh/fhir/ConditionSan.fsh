Profile: ConditionSan
Parent: Condition
Id: condition-san
Title: "Condition (SAN)"
Description: """
Generiek Santeon FHIR R4-profiel voor aandoeningen en diagnoses.

De aansluiting op het Santeon Informatiemodel wordt
vastgelegd via afzonderlijke mappings naar de
relevante bronmodellen.

De-identificatie wordt uitgevoerd volgens afzonderlijke
transformatiespecificaties en is geen inherente
eigenschap van dit profiel.
"""

// Meta overhead
* meta 0..0
* implicitRules 0..0
* language 0..0
* text 0..0
* contained 0..0
* extension 0..0
* modifierExtension 0..0

// Identificatie
* identifier 0..*
* identifier.use 0..0
* identifier.value 1..1

// Condition.identifier wordt gevuld wanneer een
// betrouwbare bronidentifier beschikbaar is.
// ProbleemID mag niet zonder onderbouwing worden
// gebruikt als identificatie van een DBC-diagnose.

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

Mapping: ConditionSanFromDBC
Id: ConditionSanFromDBC
Title: "ConditionSan vanuit SIM DBC"
Source: ConditionSan
Target: "urn:santeon:sim:csv"

// Patient
* subject -> "DBC;Identificatienummer" "Patient-reference op basis van identificatienummer."

// Diagnose
// Alleen wanneer SpecialismeDiagnose daadwerkelijk
// een klinische diagnose vertegenwoordigt.
* code.coding.system -> "DBC;SpecialismeDiagnoseSysteem"
* code.coding.code -> "DBC;SpecialismeDiagnoseCode"
* code.coding.display -> "DBC;SpecialismeDiagnoseOmschrijving"

// TODO:
// Verifiëren of SpecialismeDiagnose naar Condition.code
// mag worden gemapt, of dat DBC.Diagnose passender is.
//
// Geen identifier-mapping:
// DBC.SubtrajectNr identificeert het subtraject,
// niet automatisch de Condition.