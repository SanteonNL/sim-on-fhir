Profile: EpisodeOfCareSan
Parent: EpisodeOfCare
Id: episodeOfCare-san
Title: "EpisodeOfCare (SAN)"
Description: "Generiek Santeon-profiel voor zorgepisodes (SIMDBC), gebaseerd op FHIR R4 EpisodeOfCare (er is geen Nictiz GBB- of HL7 Europe-profiel). Datasetprofielen sluiten de elementen die zij niet gebruiken."

// Opgenomen
* identifier 0..* MS
* identifier.value 1..1
* status 1..1 MS
* status from SanEpisodeOfCareStatus (required)
* status ^short = "Afgeleid uit DBCgeldig en DBCSluitingsDatum"
* status ^comment = "Geldig = false -> entered-in-error; Geldig = true zonder SluitingsDatum -> active; Geldig = true met SluitingsDatum -> finished."
* type 0..* MS
* patient 1..1 MS
* patient only Reference(PatientSan)
* diagnosis 0..* MS
* diagnosis.condition only Reference(ConditionSan)
* period 0..1 MS

// Uitgesloten
* implicitRules 0..0
* text 0..0
* contained 0..0
* modifierExtension 0..0


ValueSet: SanEpisodeOfCareStatus
Id: san-episodeofcare-status
Title: "EpisodeOfCare status (SAN)"
Description: "De EpisodeOfCare-statussen die uit een DBC kunnen worden afgeleid: active (geldig, open), finished (geldig, gesloten) en entered-in-error (vervallen)."
* ^status = #draft
* ^experimental = false
* http://hl7.org/fhir/episode-of-care-status#active
* http://hl7.org/fhir/episode-of-care-status#finished
* http://hl7.org/fhir/episode-of-care-status#entered-in-error


Mapping: EpisodeOfCareSanFromSIMCSV
Id: sim-dbc
Title: "Santeon Informatiemodel (SIM CSV)"
Description: "Informatieve mapping van FHIR EpisodeOfCare naar SIM DBC-bronvelden."
Source: EpisodeOfCareSan
Target: "urn:santeon:sim:csv"

* identifier.system -> "DBC;SubtrajectNr" "Systeemdeel van de samengestelde identifier."
* identifier.value -> "DBC;SubtrajectNr" "Waardedeel van de samengestelde identifier; hash volgens ETL-specificatie."
* patient -> "DBC;Identificatienummer" "Reference naar PatientSan op basis van patiëntidentifier."
* type.coding.system -> "DBC;ZorgTypeCodeSysteem"
* type.coding.code -> "DBC;ZorgTypeCode"
* type.coding.display -> "DBC;ZorgTypeOmschrijving"
* period.start -> "DBC;DBCOpeningsDatum" "Alleen indien gelijk aan begin van de zorgepisode."
* period.end -> "DBC;DBCSluitingsDatum" "Alleen indien gelijk aan einde van de zorgepisode."
* status -> "DBC;DBCgeldig" "Samen met DBCSluitingsDatum: niet geldig -> entered-in-error; geldig en open -> active; geldig en gesloten -> finished."
