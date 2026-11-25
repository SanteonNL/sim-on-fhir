Logical: SIMCarePlan
Parent: Base
Id: SIMCarePlan
Title: "Zorgplan (model)"
Description: "Generiek logisch model voor een zorgplan (deelname aan een zorgpad of thuismonitoringprogramma) binnen het Santeon Informatiemodel."

// IBD-scope
* CarePlanID 1..1 Identifier "Uniek ID (key) van het zorgplan"
* PatientIdentificatienummer 1..1 Identifier "Identificatienummer van de patiënt"
* Status 0..1 code "Status van het zorgplan"
* Categorie 0..1 CodeableConcept "Zorgpad (programma) of onderliggend protocol waartoe het zorgplan behoort"
* StartDatumTijd 0..1 dateTime "Datum en eventueel tijd waarop het zorgplan ingaat (inclusiedatum)"
* EindDatumTijd 0..1 dateTime "Datum en eventueel tijd waarop het zorgplan eindigt"

// Buiten IBD-scope - nog niet meegenomen
// * Intent 0..1 code "Mate van autoriteit of intentie van het zorgplan"
// * AanmaakDatumTijd 0..1 dateTime "Datum en eventueel tijd waarop het zorgplan is vastgelegd"
// * StopReden 0..1 CodeableConcept "Reden waarom het zorgplan is gestopt"

// Terminologie
// Nog geen actieve bindingen. Te beoordelen:
// Status: in de IBD-indicatoren active (komt overeen met FHIR request-status).
// Categorie: in de IBD-indicatoren ZBJ_IBD; codestelsel uit SIM-bron vaststellen.
// StopReden: bijbehorende ValueSet controleren.



// ============================================
// MAPPING 1: Herkomst - SIM CSV
// ============================================

Mapping: SIMCarePlanFromSIMCSV
Id: SIMCarePlanFromSIMCSV
Title: "SIM CSV"
Description: "Herleidbaarheid van SIMCarePlan-elementen naar de oorspronkelijke SIM CSV-attributen."
Source: SIMCarePlan
Target: "urn:santeon:sim:csv"

// IBD-scope
* CarePlanID -> "CarePlan;CarePlanIdentifier"
* PatientIdentificatienummer -> "CarePlan;Identificatienummer"
* Status -> "CarePlan;CarePlanStatus"
* Categorie.coding.system -> "CarePlan;CarePlanCategoryCodingSystem"
* Categorie.coding.code -> "CarePlan;CarePlanCategoryCodingCode"
* Categorie.coding.display -> "CarePlan;CarePlanCategoryCodingDisplay"
* StartDatumTijd -> "CarePlan;CarePlanPeriodStart"
* EindDatumTijd -> "CarePlan;CarePlanPeriodEnd"

// Buiten IBD-scope - nog niet meegenomen
// * Intent -> "CarePlan;CarePlanIntent"
// * AanmaakDatumTijd -> "CarePlan;CarePlanCreated"
// * StopReden.coding.system -> "CarePlan;CarePlanStopReasonCodingSystem"
// * StopReden.coding.code -> "CarePlan;CarePlanStopReasonCodingCode"
// * StopReden.coding.display -> "CarePlan;CarePlanStopReasonCodingDisplay"


// ============================================
// MAPPING 2: Semantische delta-analyse - SIMCarePlan naar Nictiz GBB-afsprakenmodel
// ============================================

// Geen mapping: Nictiz GBB (referentieversie 1.0.0-alpha.2) bevat geen
// overeenkomstig afsprakenmodel voor een zorgplan.


// ============================================
// MAPPING 3: Technische vertaling, harmonisatie en conversie - SIMCarePlan naar CarePlanSan
// De omgekeerde mapping (CarePlanSan naar SIMCarePlan) wordt automatisch gegenereerd.
// ============================================

Mapping: SIMCarePlanToFHIR
Id: SIMCarePlanToFHIR
Title: "FHIR-mapping naar CarePlanSan"
Description: "Mapping van SIMCarePlan-elementen naar het Santeon FHIR-profiel CarePlanSan, inclusief harmonisatie met FHIR-standaarden en toelichting op benodigde conversies."
Source: SIMCarePlan
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/carePlan-san"

// IBD-scope
* CarePlanID -> "CarePlan.identifier" "Direct: samengestelde identifier (system|value); het waardedeel wordt gehasht volgens de ETL-specificatie."
* PatientIdentificatienummer -> "CarePlan.subject" "Structuurconversie: Patient-reference op basis van identificatienummer."
* Status -> "CarePlan.status" "Direct: SIM code wordt overgenomen als FHIR code; aansluiting op FHIR request-status nog te bevestigen."
* Categorie -> "CarePlan.category" "Direct: SIM CodeableConcept wordt overgenomen als FHIR CodeableConcept."
* StartDatumTijd -> "CarePlan.period.start" "Structuurconversie: SIM StartDatumTijd en EindDatumTijd vormen samen de FHIR Period."
* EindDatumTijd -> "CarePlan.period.end" "Structuurconversie: SIM StartDatumTijd en EindDatumTijd vormen samen de FHIR Period."

// Buiten IBD-scope - nog niet meegenomen
// * Intent -> "CarePlan.intent"
// * AanmaakDatumTijd -> "CarePlan.created"
// * StopReden -> "CarePlan.extension" "Extensie voor de stopreden nog te definiëren."


// ============================================
// Status en openstaande controles
// ============================================

// Status: In progress
// TODO: CarePlan.status is verplicht in FHIR R4, maar Status is nullable in de bron; afleidingsregel vaststellen voor records zonder status.
// TODO: CarePlan.intent is verplicht in FHIR R4. Intent valt buiten de IBD-scope; opnemen in het model of een vaste waarde afspreken.
// TODO: Terminologie voor Status, Categorie en StopReden beoordelen (zie Terminologie).
