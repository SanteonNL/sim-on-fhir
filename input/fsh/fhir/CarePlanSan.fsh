Profile: CarePlanSan
Parent: CarePlan
Id: carePlan-san
Title: "CarePlan (SAN)"
Description: "Generiek Santeon-profiel voor zorgplannen, zoals deelname aan een zorgpad of thuismonitoringprogramma (SIMCarePlan), gebaseerd op FHIR R4 CarePlan (er is geen Nictiz GBB- of HL7 Europe-profiel). Datasetprofielen sluiten de elementen die zij niet gebruiken."

// Opgenomen
* identifier 1..1 MS
* identifier.value 1..1
* status 1..1 MS
* intent 1..1 MS
* category 0..* MS
* subject 1..1 MS
* subject only Reference(PatientSan)
* period 0..1 MS

// Uitgesloten
* implicitRules 0..0
* text 0..0
* contained 0..0
* modifierExtension 0..0


Mapping: CarePlanSanFromSIMCSV
Id: sim
Title: "Santeon Informatiemodel (SIM CSV)"
Description: "Informatieve mapping van FHIR CarePlan naar SIM CSV-bronvelden."
Source: CarePlanSan
Target: "urn:santeon:sim:csv"

* identifier.system -> "CarePlan;CarePlanIdentifier" "Systeemdeel van de samengestelde identifier."
* identifier.value -> "CarePlan;CarePlanIdentifier" "Waardedeel van de samengestelde identifier; hash."
* subject -> "CarePlan;Identificatienummer" "Patient-reference op basis van patiëntidentifier."
* status -> "CarePlan;CarePlanStatus"
* intent -> "CarePlan;CarePlanIntent" "Uitgeschakeld in het SIM LM (buiten IBD-scope)."
* category.coding.system -> "CarePlan;CarePlanCategoryCodingSystem"
* category.coding.code -> "CarePlan;CarePlanCategoryCodingCode"
* category.coding.display -> "CarePlan;CarePlanCategoryCodingDisplay"
* period.start -> "CarePlan;CarePlanPeriodStart"
* period.end -> "CarePlan;CarePlanPeriodEnd"
