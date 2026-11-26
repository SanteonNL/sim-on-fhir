Logical: SIMFasering
Parent: Base
Id: SIMFasering
Title: "Fasering (model)"
Description: "Generiek logisch model voor een procesfasering binnen het Santeon Informatiemodel, bijvoorbeeld faseringen rondom een operatie."

// IBD-scope
* FaseringID 1..1 Identifier "Uniek ID (key) binnen patiënt per fasering"
* ContactID 0..1 Identifier "Uniek ID (key) binnen patiënt per contact"
* OperatieID 0..1 Identifier "Uniek ID (key) binnen patiënt per OK"
* PatientIdentificatienummer 1..1 Identifier "Identificatienummer van de patiënt"
* FaseringType 0..1 CodeableConcept "Type procesfasering"
* BeginDatumTijd 0..1 dateTime "Datum en eventueel tijd waarop de fasering begint"
* EindDatumTijd 0..1 dateTime "Datum en eventueel tijd waarop de fasering eindigt"

// Terminologie
// Nog geen actieve bindingen. Te beoordelen:
// FaseringType: Santeon_OKTijdTypeCodelijst.



// ============================================
// MAPPING 1: Herkomst - SIM CSV
// ============================================

Mapping: SIMFaseringFromSIMCSV
Id: SIMFaseringFromSIMCSV
Title: "SIM CSV"
Description: "Herleidbaarheid van SIMFasering-elementen naar de oorspronkelijke SIM CSV-attributen."
Source: SIMFasering
Target: "urn:santeon:sim:csv"

// IBD-scope
* FaseringID -> "Fasering;FaseringID"
* ContactID -> "Fasering;ContactID"
* OperatieID -> "Fasering;OperatieID"
* PatientIdentificatienummer -> "Fasering;Identificatienummer"
* FaseringType.coding.system -> "Fasering;FaseringCodeSysteem"
* FaseringType.coding.code -> "Fasering;FaseringCode"
* FaseringType.coding.display -> "Fasering;FaseringOmschrijving"
* BeginDatumTijd -> "Fasering;FaseringBeginDatumTijd"
* EindDatumTijd -> "Fasering;FaseringEindDatumTijd"


// ============================================
// MAPPING 2: Semantische delta-analyse - SIMFasering naar Nictiz GBB-afsprakenmodel
// ============================================

// Geen mapping: Nictiz GBB (referentieversie 1.0.0-alpha.2) bevat geen
// overeenkomstig afsprakenmodel voor een procesfasering.
// Ook in 1.0.0-alpha.1 was dit niet beschikbaar.


// ============================================
// MAPPING 3: Technische vertaling, harmonisatie en conversie - SIMFasering naar Santeon FHIR-profiel
// ============================================

// Geen mapping: het FHIR-doelprofiel is nog niet vastgesteld. De eerdere
// mapping naar EncounterSan is semantisch nog niet voldoende onderbouwd.
// ContactID en OperatieID blijven behouden om de relaties te kunnen reconstrueren.


// ============================================
// Status en openstaande controles
// ============================================

// Status: In progress
// TODO: FHIR-doelresource vaststellen:
//   - Encounter, indien de fase een zorgcontact is;
//   - Procedure, indien de fase een verrichting is;
//   - Task, indien de fase een workflowactiviteit is;
//   - Observation, indien de fase als observatie of geregistreerde gebeurtenis wordt vastgelegd.
// TODO: Terminologie voor FaseringType beoordelen (zie Terminologie).
// TODO: GBB-delta-analyse uitvoeren zodra Nictiz een passend procesfasering-afsprakenmodel publiceert.
