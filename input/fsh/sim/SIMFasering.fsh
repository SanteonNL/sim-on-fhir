Logical: SIMFasering
Parent: Base
// Parent: GBB - nog te beoordelen
Id: SIMFasering
Title: "Fasering (model)"
Description: "Generiek logisch model voor een procesfasering binnen het Santeon Informatiemodel, bijvoorbeeld faseringen rondom een operatie."

// IBD
* FaseringID 1..1 Identifier "Uniek ID (key) binnen patiënt per fasering"
* ContactID 0..1 Identifier "Uniek ID (key) binnen patiënt per contact"
* OperatieID 0..1 Identifier "Uniek ID (key) binnen patiënt per OK"
* PatientIdentificatienummer 1..1 Identifier "Identificatienummer van de patiënt"

* FaseringType 0..1 CodeableConcept "Type procesfasering"

* BeginDatumTijd 0..1 dateTime "Datum en eventueel tijd waarop de fasering begint"
* EindDatumTijd 0..1 dateTime "Datum en eventueel tijd waarop de fasering eindigt"

// Terminologie nog te beoordelen:
// FaseringType: Santeon_OKTijdTypeCodelijst


// Mapping vanuit SIM CSV
Mapping: SIMFaseringFromSIMCSV
Id: SIMFaseringFromSIMCSV
Title: "SIM CSV"
Source: SIMFasering
Target: "urn:santeon:sim:csv"

// Identificatie en relaties
* FaseringID -> "Fasering;FaseringID"
* ContactID -> "Fasering;ContactID"
* OperatieID -> "Fasering;OperatieID"
* PatientIdentificatienummer -> "Fasering;Identificatienummer"

// Type fasering
* FaseringType.coding.system -> "Fasering;FaseringCodeSysteem"
* FaseringType.coding.code -> "Fasering;FaseringCode"
* FaseringType.coding.display -> "Fasering;FaseringOmschrijving"

// Tijdstippen
* BeginDatumTijd -> "Fasering;FaseringBeginDatumTijd"
* EindDatumTijd -> "Fasering;FaseringEindDatumTijd"


// Mapping naar FHIR
// Nog niet definitief vastgesteld.
//
// De eerdere mapping naar EncounterSan is
// semantisch nog niet voldoende onderbouwd.
//
// Te beoordelen:
// - Encounter, indien de fase een zorgcontact is
// - Procedure, indien de fase een verrichting is
// - Task, indien de fase een workflowactiviteit is
// - Observation, indien de fase als observatie
//   of geregistreerde gebeurtenis wordt vastgelegd
//
// ContactID en OperatieID blijven behouden
// om de relaties te kunnen reconstrueren.


// GBB: geen overeenkomstig Logical Model beschikbaar
// in Nictiz GBB 1.0.0-alpha.1.
// GBB-mapping toevoegen zodra een passend generiek
// Fasering/procesfasering-model is gepubliceerd.