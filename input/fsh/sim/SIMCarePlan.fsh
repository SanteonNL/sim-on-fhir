Logical: SIMCarePlan
Parent: Base
// Parent: GBB - nog te beoordelen
Id: SIMCarePlan
Title: "Zorgplan (model)"
Description: "Generiek logisch model voor een zorgplan (deelname aan een zorgpad of thuismonitoringprogramma) binnen het Santeon Informatiemodel."

// IBD
* CarePlanID 1..1 Identifier "Uniek ID (key) van het zorgplan"
* PatientIdentificatienummer 1..1 Identifier "Identificatienummer van de patiënt"

* Status 0..1 code "Status van het zorgplan"
* Categorie 0..1 CodeableConcept "Zorgpad (programma) of onderliggend protocol waartoe het zorgplan behoort"

* StartDatumTijd 0..1 dateTime "Datum en eventueel tijd waarop het zorgplan ingaat (inclusiedatum)"
* EindDatumTijd 0..1 dateTime "Datum en eventueel tijd waarop het zorgplan eindigt"

// Overig van model CarePlan
// * Intent 0..1 code "Mate van autoriteit of intentie van het zorgplan"
// * AanmaakDatumTijd 0..1 dateTime "Datum en eventueel tijd waarop het zorgplan is vastgelegd"
// * StopReden 0..1 CodeableConcept "Reden waarom het zorgplan is gestopt"

// Terminologie nog te beoordelen:
// Status: in de IBD-indicatoren active (komt overeen met FHIR request-status)
// Categorie: in de IBD-indicatoren ZBJ_IBD; codestelsel uit SIM-bron vaststellen
// StopReden: bijbehorende ValueSet controleren


// Mapping vanuit SIM CSV
Mapping: SIMCarePlanFromSIMCSV
Id: SIMCarePlanFromSIMCSV
Title: "SIM CSV"
Source: SIMCarePlan
Target: "urn:santeon:sim:csv"

// Identificatie
* CarePlanID -> "CarePlan;CarePlanIdentifier"
* PatientIdentificatienummer -> "CarePlan;Identificatienummer"

// Status
* Status -> "CarePlan;CarePlanStatus"

// Categorie
* Categorie.coding.system -> "CarePlan;CarePlanCategoryCodingSystem"
* Categorie.coding.code -> "CarePlan;CarePlanCategoryCodingCode"
* Categorie.coding.display -> "CarePlan;CarePlanCategoryCodingDisplay"

// Periode
* StartDatumTijd -> "CarePlan;CarePlanPeriodStart"
* EindDatumTijd -> "CarePlan;CarePlanPeriodEnd"

// Overig van model CarePlan
// * Intent -> "CarePlan;CarePlanIntent"
// * AanmaakDatumTijd -> "CarePlan;CarePlanCreated"
// * StopReden.coding.system -> "CarePlan;CarePlanStopReasonCodingSystem"
// * StopReden.coding.code -> "CarePlan;CarePlanStopReasonCodingCode"
// * StopReden.coding.display -> "CarePlan;CarePlanStopReasonCodingDisplay"


// Mapping naar FHIR

Mapping: SIMCarePlanToFHIR
Id: SIMCarePlanToFHIR
Title: "SIMCarePlan naar FHIR"
Source: SIMCarePlan
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/carePlan-san"

// Identificatie
* CarePlanID -> "CarePlan.identifier"
* PatientIdentificatienummer -> "CarePlan.subject" "Patient-reference op basis van identificatienummer."

// Zorgplan
* Status -> "CarePlan.status"
* Categorie -> "CarePlan.category"

// Periode
* StartDatumTijd -> "CarePlan.period.start"
* EindDatumTijd -> "CarePlan.period.end"

// Overig van model CarePlan
// * Intent -> "CarePlan.intent"
// * AanmaakDatumTijd -> "CarePlan.created"
// * StopReden -> "CarePlan.extension" "Extensie voor de stopreden nog te definiëren."

// Nog uit te werken:
//
// CarePlan.status: verplicht in FHIR R4, maar Status is nullable in de bron.
//
// CarePlan.intent: verplicht in FHIR R4. Intent valt buiten de IBD-scope;
// opnemen in het model of een vaste waarde afspreken.

// GBB: nog te beoordelen of Nictiz GBB een overeenkomstig
// Logical Model voor een zorgplan biedt.
