
Profile: CarePlanSan
Parent: CarePlan
Id: carePlan-san
Title: "CarePlan (SAN)"
Description: """
Generiek Santeon FHIR R4-profiel voor zorgplannen, zoals deelname
aan een zorgpad of thuismonitoringprogramma.

Gebaseerd op het Santeon Informatiemodel (SIMCarePlan).

De-identificatie wordt uitgevoerd volgens afzonderlijke
transformatiespecificaties en is geen inherente
eigenschap van dit profiel.
"""

// Identifier (SIMCarePlan.CarePlanID)
* identifier 1..1
* identifier.use 0..0
* identifier.value 1..1
// identifier.system blijft toegestaan, omdat het SIM
// identifier_system|identifier_value ondersteunt.

// Status
// CarePlan.status is verplicht in FHIR R4 (1..1),
// maar Status is nullable in de bron.
// TODO: afleidingsregel vaststellen voor records zonder status.

// CarePlan.intent is verplicht in FHIR R4 (1..1).
// Intent staat uitgeschakeld in het SIM LM (buiten IBD-scope).
// TODO: Intent opnemen in het LM of een vaste waarde vaststellen.

// Categorie
* category 0..*

// PatientIdentificatienummer
* subject 1..1
* subject only Reference(PatientSan)

// StartDatumTijd / EindDatumTijd
* period 0..1
* period.start 0..1
* period.end 0..1

// Overige FHIR-elementen blijven beschikbaar.
// Toepassingsspecifieke beperkingen kunnen
// in een afgeleid profiel worden opgenomen.


Mapping: CarePlanSanFromSIMCSV
Id: sim
Title: "Santeon Informatiemodel (SIM CSV)"
Description: "Informatieve mapping van FHIR CarePlan naar SIM CSV-bronvelden."
Source: CarePlanSan
Target: "urn:santeon:sim:csv"

// Identificatie
* identifier.system -> "CarePlan;CarePlanIdentifier" "Systeemdeel van de samengestelde identifier."
* identifier.value -> "CarePlan;CarePlanIdentifier" "Waardedeel van de samengestelde identifier; hash."

* subject -> "CarePlan;Identificatienummer" "Patient-reference op basis van patiëntidentifier."

// Status en intent
* status -> "CarePlan;CarePlanStatus"
* intent -> "CarePlan;CarePlanIntent" "Uitgeschakeld in het SIM LM (buiten IBD-scope)."

// Categorie
* category.coding.system -> "CarePlan;CarePlanCategoryCodingSystem"
* category.coding.code -> "CarePlan;CarePlanCategoryCodingCode"
* category.coding.display -> "CarePlan;CarePlanCategoryCodingDisplay"

// Periode
* period.start -> "CarePlan;CarePlanPeriodStart"
* period.end -> "CarePlan;CarePlanPeriodEnd"

// Nog uit te werken:
// CarePlanCreated: CarePlan.created (uitgeschakeld in LM).
// CarePlanStopReasonCoding*: extensie nog te definiëren
// (uitgeschakeld in LM).
