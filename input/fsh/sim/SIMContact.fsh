
Logical: SIMContact
Parent: Base
// Parent: GBB - nog te beoordelen
Id: SIMContact
Title: "Contact (model)"
Description: "Generiek logisch model voor een zorgcontact binnen het Santeon Informatiemodel."

// IBD
* ContactID 1..1 Identifier "Uniek ID (key) binnen patiënt per contact"
* VerrichtingID 0..1 Identifier "Uniek ID (key) binnen patiënt per verrichting"
* SubtrajectNummer 0..1 Identifier "Uniek ID (key) binnen patiënt per DBC-subtraject"
* PatientIdentificatienummer 1..1 Identifier "Identificatienummer van de patiënt"

* ContactType 0..1 Coding "Type zorgcontact, bijvoorbeeld poliklinisch, klinisch of spoedeisend"
* ContactPrioriteit 0..1 CodeableConcept "Urgentie van het zorgcontact"
* ContactPrioriteit from http://terminology.hl7.org/ValueSet/v3-ActPriority (example)

* Bestemming 0..1 CodeableConcept "Bestemming van de patiënt na het zorgcontact"

* BeginDatumTijd 0..1 dateTime "Datum en eventueel tijd waarop het contact is begonnen"
* EindDatumTijd 0..1 dateTime "Datum en eventueel tijd waarop het contact is beëindigd"

* ContactMet 0..1 Coding "Specialisme dat verantwoordelijk is voor het contact"
* AfspraakID 0..1 Identifier "Identificatienummer van de afspraak"

// Overig van model Contact
// * ProbleemID 0..1 Identifier "Identificatienummer van het gerelateerde probleem"
// * Herkomst 0..1 CodeableConcept "Herkomst van de patiënt voorafgaand aan het zorgcontact"

// * ZorgaanbiederIdentificatienummer 0..1 Identifier "Identificatienummer van de verantwoordelijke zorgaanbieder"
// * OrganisatieNaam 0..1 string "Naam van de zorgaanbieder"
//* OrganisatieLocatie 0..1 string "Naam van de locatie waar het zorgcontact plaatsvindt"

// TO DO:
// Terminologie nog te beoordelen:
// ContactType: Santeon_ContactTypeCodelijst
// ContactPrioriteit: ActPriority
// ContactMet: SpecialismeAGBCodelijst
// Herkomst: HerkomstCodelijst controleren
// Bestemming: BestemmingCodelijst controleren


Mapping: SIMContactFromSIMCSV
Id: SIMContactFromSIMCSV
Title: "SIM CSV"
Source: SIMContact
Target: "urn:santeon:sim:csv"

// Identificatie en relaties
* ContactID -> "Contact;ContactID"
* VerrichtingID -> "Contact;VerrichtingID"
* SubtrajectNummer -> "Contact;SubtrajectNr"
* PatientIdentificatienummer -> "Contact;Identificatienummer"

// ContactType
* ContactType.system -> "Contact;ContactTypeCodeSysteem"
* ContactType.code -> "Contact;ContactTypeCode"
* ContactType.display -> "Contact;ContactTypeOmschrijving"

// ContactPrioriteit
* ContactPrioriteit.coding.system -> "Contact;EncounterPriorityCodingSystem"
* ContactPrioriteit.coding.code -> "Contact;EncounterPriorityCodingCode"
* ContactPrioriteit.coding.display -> "Contact;EncounterPriorityCodingDisplay"

// Bestemming
* Bestemming.coding.system -> "Contact;BestemmingCodeSysteem"
* Bestemming.coding.code -> "Contact;BestemmingCode"
* Bestemming.coding.display -> "Contact;BestemmingOmschrijving"

// Periode
* BeginDatumTijd -> "Contact;BeginDatumTijd"
* EindDatumTijd -> "Contact;EindDatumTijd"

// Specialisme
* ContactMet.system -> "Contact;ContactMetSysteem"
* ContactMet.code -> "Contact;ContactMet"

// Afspraak
* AfspraakID -> "Contact;AfspraakID"

// Overig van model Contact
// * ProbleemID -> "Contact;ProbleemID"

// * Herkomst.coding.system -> "Contact;HerkomstCodeSysteem"
// * Herkomst.coding.code -> "Contact;HerkomstCode"
// * Herkomst.coding.display -> "Contact;HerkomstOmschrijving"

// * ZorgaanbiederIdentificatienummer -> "Contact;ZorgaanbiederIdentificatienummer"
// * OrganisatieNaam -> "Contact;OrganisatieNaam"
// * OrganisatieLocatie -> "Contact;OrganisatieLocatie"


Mapping: SIMContactToFHIR
Id: SIMContactToFHIR
Title: "SIMContact naar FHIR"
Source: SIMContact
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/encounter-san"


// Identificatie en relaties
* ContactID -> "Encounter.identifier"
* PatientIdentificatienummer -> "Encounter.subject" "Patient-reference op basis van identificatienummer."
* SubtrajectNummer -> "Encounter.episodeOfCare" "Indien het DBC-subtraject als EpisodeOfCare wordt gerepresenteerd."

// Contactgegevens
* ContactType -> "Encounter.class"
* ContactPrioriteit -> "Encounter.priority"
* Bestemming -> "Encounter.hospitalization.dischargeDisposition" "Wanneer het gegeven de ontslagbestemming beschrijft."

// Periode
* BeginDatumTijd -> "Encounter.period.start"
* EindDatumTijd -> "Encounter.period.end"

// Afspraak
* AfspraakID -> "Encounter.appointment" "Appointment-reference op basis van AfspraakID."

// Overig van model Contact
// * Herkomst -> "Encounter.hospitalization.admitSource" "Wanneer het gegeven de opnameherkomst beschrijft."
// * ZorgaanbiederIdentificatienummer -> "Encounter.serviceProvider" "Organization-reference op basis van identificatienummer."

// Nog uit te werken:
//
// Encounter.status: verplicht in FHIR R4.
// Herkomst of gevalideerde afleidingsregel vaststellen.
//
// Encounter.class: verplicht in FHIR R4,
// maar ContactType is nullable in de bron.
//
// VerrichtingID:
// Geen directe mapping tenzij de verrichting
// daadwerkelijk de reden voor het contact is.
// Relatie loopt primair via Procedure.encounter.
//
// ProbleemID:
// Eventueel Encounter.reasonReference naar Condition,
// alleen indien het probleem de reden voor het contact is.
//
// ContactMet:
// Via PractitionerRole.specialty indien een passende
// PractitionerRole kan worden geïdentificeerd.
//
// OrganisatieNaam:
// Via Organization.name.
//
// OrganisatieLocatie:
// Via Location.name, met een passende Location-reference
// in Encounter.location.location.

// GBB: geen overeenkomstig Logical Model beschikbaar
// in Nictiz GBB 1.0.0-alpha.1.
// GBB-mapping toevoegen zodra een passend generiek
// Encounter-model is gepubliceerd.