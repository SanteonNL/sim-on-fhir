Profile: EncounterSan
Parent: Encounter
Id: encounter-san
Title: "Encounter (SAN)"
Description: "Generiek Santeon-profiel voor zorgcontacten (SIMContact), gebaseerd op FHIR R4 Encounter (er is geen Nictiz GBB- of HL7 Europe-profiel). Datasetprofielen sluiten de elementen die zij niet gebruiken."

// Opgenomen
* identifier 1..1 MS
* identifier.value 1..1
* status 1..1 MS
* class 1..1 MS
* priority 0..1 MS
* subject 1..1 MS
* subject only Reference(PatientSan)
* episodeOfCare 0..* MS
* episodeOfCare only Reference(EpisodeOfCareSan)
* period 0..1 MS
* hospitalization.dischargeDisposition 0..1 MS
* appointment 0..* MS

// Uitgesloten
* implicitRules 0..0
* text 0..0
* contained 0..0
* modifierExtension 0..0


Mapping: EncounterSanFromSIMCSV
Id: sim
Title: "Santeon Informatiemodel (SIM CSV)"
Description: "Informatieve mapping van FHIR Encounter naar SIM Contact-bronvelden."
Source: EncounterSan
Target: "urn:santeon:sim:csv"

* identifier.system -> "Contact;ContactID" "Systeemdeel van identifier_system|identifier_value."
* identifier.value -> "Contact;ContactID" "Waardedeel van de identifier; hash volgens ETL-specificatie."
* subject -> "Contact;Identificatienummer" "Patient-reference op basis van de patiëntidentifier."
* class.system -> "Contact;ContactTypeCodeSysteem"
* class.code -> "Contact;ContactTypeCode"
* class.display -> "Contact;ContactTypeOmschrijving"
* priority.coding.system -> "Contact;EncounterPriorityCodingSystem"
* priority.coding.code -> "Contact;EncounterPriorityCodingCode"
* priority.coding.display -> "Contact;EncounterPriorityCodingDisplay"
* hospitalization.dischargeDisposition.coding.system -> "Contact;BestemmingCodeSysteem" "Alleen bij ontslagbestemming."
* hospitalization.dischargeDisposition.coding.code -> "Contact;BestemmingCode" "Alleen bij ontslagbestemming."
* hospitalization.dischargeDisposition.coding.display -> "Contact;BestemmingOmschrijving" "Alleen bij ontslagbestemming."
* period.start -> "Contact;BeginDatumTijd" "Dateshift volgens afzonderlijke transformatiespecificatie."
* period.end -> "Contact;EindDatumTijd" "Dateshift volgens afzonderlijke transformatiespecificatie."
* episodeOfCare -> "Contact;SubtrajectNr" "Alleen indien DBC-subtraject als EpisodeOfCareSan wordt gerepresenteerd."
* appointment -> "Contact;AfspraakID" "Reference naar Appointment op basis van AfspraakID."
