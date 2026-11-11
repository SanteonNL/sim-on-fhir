
Profile: EncounterSan
Parent: Encounter
Id: encounter-san
Title: "Encounter (SAN)"
Description: """
Generiek Santeon FHIR R4-profiel voor zorgcontacten.

Gebaseerd op het Santeon Informatiemodel (SIMContact).

De-identificatie wordt uitgevoerd volgens afzonderlijke
transformatiespecificaties en is geen inherente
eigenschap van dit profiel.
"""

// ContactID: verplichte bronidentifier.
// Het SIM gebruikt identifier_system|identifier_value.
* identifier 1..1
* identifier.value 1..1

// Verplicht vanuit FHIR R4, maar geen zelfstandig
// statusveld beschikbaar in SIMContact.
// TODO: herkomst of gevalideerde ETL2-afleidingsregel vaststellen.
* status 1..1

// ContactType: Coding; verplicht in FHIR R4,
// terwijl de SIM-bron nullwaarden toestaat.
// TODO: expliciete afspraak over ontbrekend ContactType.
* class 1..1

// PatientIdentificatienummer
* subject 1..1
* subject only Reference(PatientSan)

// ContactPrioriteit
* priority 0..1
* priority from http://terminology.hl7.org/ValueSet/v3-ActPriority (example)

// SubtrajectNummer: alleen wanneer een DBC-subtraject
// als EpisodeOfCareSan wordt gerepresenteerd.
* episodeOfCare only Reference(EpisodeOfCareSan)

// BeginDatumTijd en EindDatumTijd
* period 0..1
* period.start 0..1
* period.end 0..1

// Bestemming: alleen indien het een ontslagbestemming betreft.
// hospitalization en dischargeDisposition zijn optioneel.
* hospitalization.dischargeDisposition 0..1

// AfspraakID: de base Encounter.appointment verwijst naar Appointment.
// Er is nog geen AppointmentSan-profiel.

// ContactMet: alleen bij een passend identificeerbare
// PractitionerRole via Encounter.participant.individual.
// Geen directe mapping van specialismecode naar een Reference.

// Overige FHIR-elementen blijven beschikbaar.
// Geen 0..0-beperkingen uitsluitend vanwege de huidige IBD-scope.


Mapping: EncounterSanFromSIMCSV
Id: sim
Title: "Santeon Informatiemodel (SIM CSV)"
Description: "Informatieve mapping van FHIR Encounter naar SIM Contact-bronvelden."
Source: EncounterSan
Target: "urn:santeon:sim:csv"

// ContactID: twee onderdelen van dezelfde samengestelde bronidentifier.
* identifier.system -> "Contact;ContactID" "Systeemdeel van identifier_system|identifier_value."
* identifier.value -> "Contact;ContactID" "Waardedeel van de identifier; hash volgens ETL-specificatie."

// Patient
* subject -> "Contact;Identificatienummer" "Patient-reference op basis van de patiëntidentifier."

// ContactType
* class.system -> "Contact;ContactTypeCodeSysteem"
* class.code -> "Contact;ContactTypeCode"
* class.display -> "Contact;ContactTypeOmschrijving"

// ContactPrioriteit
* priority.coding.system -> "Contact;EncounterPriorityCodingSystem"
* priority.coding.code -> "Contact;EncounterPriorityCodingCode"
* priority.coding.display -> "Contact;EncounterPriorityCodingDisplay"

// Bestemming
* hospitalization.dischargeDisposition.coding.system -> "Contact;BestemmingCodeSysteem" "Alleen bij ontslagbestemming."
* hospitalization.dischargeDisposition.coding.code -> "Contact;BestemmingCode" "Alleen bij ontslagbestemming."
* hospitalization.dischargeDisposition.coding.display -> "Contact;BestemmingOmschrijving" "Alleen bij ontslagbestemming."

// Contactperiode
* period.start -> "Contact;BeginDatumTijd" "Dateshift volgens afzonderlijke transformatiespecificatie."
* period.end -> "Contact;EindDatumTijd" "Dateshift volgens afzonderlijke transformatiespecificatie."

// Relaties
* episodeOfCare -> "Contact;SubtrajectNr" "Alleen indien DBC-subtraject als EpisodeOfCareSan wordt gerepresenteerd."
* appointment -> "Contact;AfspraakID" "Reference naar Appointment op basis van AfspraakID."

// Geen directe Encounter-mapping voor:
// - VerrichtingID: relatie primair via Procedure.encounter.
// - ContactMet: specialismecode; alleen met passende PractitionerRole.
// - Encounter.status: verplicht maar geen expliciete SIM-bronkolom.
// Overige SIMContact-elementen zijn momenteel uitgeschakeld in het LM.
