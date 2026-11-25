Logical: SIMContact
Parent: Base
Id: SIMContact
Title: "Contact (model)"
Description: "Generiek logisch model voor een zorgcontact binnen het Santeon Informatiemodel."

// IBD-scope
* ContactID 1..1 Identifier "Uniek ID (key) binnen patiënt per contact"
* VerrichtingID 0..1 Identifier "Uniek ID (key) binnen patiënt per verrichting"
* SubtrajectNummer 0..1 Identifier "Uniek ID (key) binnen patiënt per DBC-subtraject"
* PatientIdentificatienummer 1..1 Identifier "Identificatienummer van de patiënt"
* ContactType 0..1 Coding "Type zorgcontact, bijvoorbeeld poliklinisch, klinisch of spoedeisend"
* ContactPrioriteit 0..1 CodeableConcept "Urgentie van het zorgcontact"
* Bestemming 0..1 CodeableConcept "Bestemming van de patiënt na het zorgcontact"
* BeginDatumTijd 0..1 dateTime "Datum en eventueel tijd waarop het contact is begonnen"
* EindDatumTijd 0..1 dateTime "Datum en eventueel tijd waarop het contact is beëindigd"
* ContactMet 0..1 Coding "Specialisme dat verantwoordelijk is voor het contact"

// Buiten IBD-scope - nog niet meegenomen
// * AfspraakID 0..1 Identifier "Identificatienummer van de afspraak"
// * ProbleemID 0..1 Identifier "Identificatienummer van het gerelateerde probleem"
// * Herkomst 0..1 CodeableConcept "Herkomst van de patiënt voorafgaand aan het zorgcontact"
// * ZorgaanbiederIdentificatienummer 0..1 Identifier "Identificatienummer van de verantwoordelijke zorgaanbieder"
// * OrganisatieNaam 0..1 string "Naam van de zorgaanbieder"
// * OrganisatieLocatie 0..1 string "Naam van de locatie waar het zorgcontact plaatsvindt"

// Terminologie
* ContactPrioriteit from http://terminology.hl7.org/ValueSet/v3-ActPriority (example)
// Nog te beoordelen:
// ContactType: Santeon_ContactTypeCodelijst.
// ContactMet: SpecialismeAGBCodelijst.
// Bestemming: BestemmingCodelijst controleren.
// Herkomst: HerkomstCodelijst controleren.



// ============================================
// MAPPING 1: Herkomst - SIM CSV
// ============================================

Mapping: SIMContactFromSIMCSV
Id: SIMContactFromSIMCSV
Title: "SIM CSV"
Description: "Herleidbaarheid van SIMContact-elementen naar de oorspronkelijke SIM CSV-attributen."
Source: SIMContact
Target: "urn:santeon:sim:csv"

// IBD-scope
* ContactID -> "Contact;ContactID"
* VerrichtingID -> "Contact;VerrichtingID"
* SubtrajectNummer -> "Contact;SubtrajectNr"
* PatientIdentificatienummer -> "Contact;Identificatienummer"
* ContactType.system -> "Contact;ContactTypeCodeSysteem"
* ContactType.code -> "Contact;ContactTypeCode"
* ContactType.display -> "Contact;ContactTypeOmschrijving"
* ContactPrioriteit.coding.system -> "Contact;EncounterPriorityCodingSystem"
* ContactPrioriteit.coding.code -> "Contact;EncounterPriorityCodingCode"
* ContactPrioriteit.coding.display -> "Contact;EncounterPriorityCodingDisplay"
* Bestemming.coding.system -> "Contact;BestemmingCodeSysteem"
* Bestemming.coding.code -> "Contact;BestemmingCode"
* Bestemming.coding.display -> "Contact;BestemmingOmschrijving"
* BeginDatumTijd -> "Contact;BeginDatumTijd"
* EindDatumTijd -> "Contact;EindDatumTijd"
* ContactMet.system -> "Contact;ContactMetSysteem"
* ContactMet.code -> "Contact;ContactMet"

// Buiten IBD-scope - nog niet meegenomen
// * AfspraakID -> "Contact;AfspraakID"
// * ProbleemID -> "Contact;ProbleemID"
// * Herkomst.coding.system -> "Contact;HerkomstCodeSysteem"
// * Herkomst.coding.code -> "Contact;HerkomstCode"
// * Herkomst.coding.display -> "Contact;HerkomstOmschrijving"
// * ZorgaanbiederIdentificatienummer -> "Contact;ZorgaanbiederIdentificatienummer"
// * OrganisatieNaam -> "Contact;OrganisatieNaam"
// * OrganisatieLocatie -> "Contact;OrganisatieLocatie"


// ============================================
// MAPPING 2: Semantische delta-analyse - SIMContact naar Nictiz GBB-afsprakenmodel
// ============================================

// Geen mapping: Nictiz GBB (referentieversie 1.0.0-alpha.2) bevat geen
// overeenkomstig afsprakenmodel voor een zorgcontact (Encounter).
// Ook in 1.0.0-alpha.1 was dit niet beschikbaar.


// ============================================
// MAPPING 3: Technische vertaling, harmonisatie en conversie - SIMContact naar EncounterSan
// De omgekeerde mapping (EncounterSan naar SIMContact) wordt automatisch gegenereerd.
// ============================================

Mapping: SIMContactToFHIR
Id: SIMContactToFHIR
Title: "FHIR-mapping naar EncounterSan"
Description: "Mapping van SIMContact-elementen naar het Santeon FHIR-profiel EncounterSan, inclusief harmonisatie met FHIR-standaarden en toelichting op benodigde conversies."
Source: SIMContact
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/encounter-san"

// IBD-scope
* ContactID -> "Encounter.identifier" "Direct: samengestelde identifier (system|value); het waardedeel wordt gehasht volgens de ETL-specificatie."
* PatientIdentificatienummer -> "Encounter.subject" "Structuurconversie: Patient-reference op basis van identificatienummer."
* SubtrajectNummer -> "Encounter.episodeOfCare" "Structuurconversie: EpisodeOfCare-reference op basis van SubtrajectNummer, indien het DBC-subtraject als EpisodeOfCare wordt gerepresenteerd."
* ContactType -> "Encounter.class" "Direct: SIM Coding wordt overgenomen als FHIR Coding; aansluiting van Santeon_ContactTypeCodelijst op de FHIR-binding nog te beoordelen."
* ContactPrioriteit -> "Encounter.priority" "Direct: SIM en FHIR gebruiken beide v3-ActPriority (example)."
* Bestemming -> "Encounter.hospitalization.dischargeDisposition" "Direct: wanneer het gegeven de ontslagbestemming beschrijft."
* BeginDatumTijd -> "Encounter.period.start" "Structuurconversie: SIM BeginDatumTijd en EindDatumTijd vormen samen de FHIR Period; dateshift volgens afzonderlijke transformatiespecificatie."
* EindDatumTijd -> "Encounter.period.end" "Structuurconversie: SIM BeginDatumTijd en EindDatumTijd vormen samen de FHIR Period; dateshift volgens afzonderlijke transformatiespecificatie."

// Buiten IBD-scope - nog niet meegenomen
// * AfspraakID -> "Encounter.appointment" "Appointment-reference op basis van AfspraakID."
// * Herkomst -> "Encounter.hospitalization.admitSource" "Wanneer het gegeven de opnameherkomst beschrijft."
// * ZorgaanbiederIdentificatienummer -> "Encounter.serviceProvider" "Organization-reference op basis van identificatienummer."


// ============================================
// Status en openstaande controles
// ============================================

// Status: In progress
// TODO: Encounter.status is verplicht in FHIR R4; herkomst of gevalideerde afleidingsregel vaststellen.
// TODO: Encounter.class is verplicht in FHIR R4, maar ContactType is nullable in de bron.
// TODO: VerrichtingID: geen directe mapping tenzij de verrichting daadwerkelijk de reden voor het contact is; de relatie loopt primair via Procedure.encounter.
// TODO: ContactMet: via PractitionerRole.specialty indien een passende PractitionerRole kan worden geïdentificeerd.
// TODO: ProbleemID (buiten IBD-scope): eventueel Encounter.reasonReference naar Condition, alleen indien het probleem de reden voor het contact is.
// TODO: OrganisatieNaam en OrganisatieLocatie (buiten IBD-scope): via Organization.name en Location.name, met een passende Location-reference in Encounter.location.location.
// TODO: Terminologie voor ContactType, ContactMet, Bestemming en Herkomst beoordelen (zie Terminologie).
// TODO: GBB-delta-analyse uitvoeren zodra Nictiz een passend generiek Encounter-afsprakenmodel publiceert.
