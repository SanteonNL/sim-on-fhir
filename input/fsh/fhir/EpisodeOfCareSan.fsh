
Profile: EpisodeOfCareSan
Parent: EpisodeOfCare
Id: episodeOfCare-san
Title: "EpisodeOfCare (SAN)"
Description: """
Generiek Santeon FHIR R4-profiel voor zorgepisodes.

Gebaseerd op het Santeon Informatiemodel (SIMDBC), waarbij
nog moet worden vastgesteld hoe een DBC-subtraject zich
verhoudt tot een FHIR EpisodeOfCare.

De-identificatie wordt uitgevoerd volgens afzonderlijke
transformatiespecificaties en is geen inherente
eigenschap van dit profiel.
"""

// Identificatie: SIMDBC.SubtrajectNummer is optioneel.
// FHIR staat meerdere identifiers toe; niet verplicht stellen.
* identifier 0..*
* identifier.value 1..1

// EpisodeOfCare.status is verplicht vanuit FHIR R4 (1..1).
// TODO: herkomst of gevalideerde ETL2-afleidingsregel bepalen.
// DBCgeldig is administratieve geldigheid en geen EpisodeOfCare.status.

// PatientIdentificatienummer
* patient 1..1
* patient only Reference(PatientSan)

// ZorgType: optioneel en alleen gebruiken wanneer het
// zorgtype de zorgepisode inhoudelijk correct classificeert.
* type 0..*

// Diagnose
// Een zorgepisode kan verwijzen naar een of meer
// ConditionSan-resources met relevante diagnoses.
* diagnosis 0..*
* diagnosis.condition only Reference(ConditionSan)
// TODO:
// Vaststellen welke SIM-diagnose wordt gerepresenteerd
// in ConditionSan.code en hoe de bijbehorende
// Condition-resource wordt geïdentificeerd.

// OpeningsDatum / SluitingsDatum zijn optioneel in SIMDBC.
// Alleen mappen wanneer de administratieve DBC-periode
// overeenkomt met de betekenis van EpisodeOfCare.period.
* period 0..1
* period.start 0..1
* period.end 0..1

// Overige FHIR-elementen blijven beschikbaar.
// Toepassingsspecifieke 0..0-beperkingen kunnen later in
// een afgeleid profiel worden opgenomen.


Mapping: EpisodeOfCareSanFromSIMCSV
Id: sim-dbc
Title: "Santeon Informatiemodel (SIM CSV)"
Description: "Informatieve mapping van FHIR EpisodeOfCare naar SIM DBC-bronvelden."
Source: EpisodeOfCareSan
Target: "urn:santeon:sim:csv"

// SubtrajectNummer: alleen bij één EpisodeOfCare per DBC-subtraject.
// De bron gebruikt identifier_system|identifier_value.
* identifier.system -> "DBC;SubtrajectNr" "Systeemdeel van de samengestelde identifier."
* identifier.value -> "DBC;SubtrajectNr" "Waardedeel van de samengestelde identifier; hash volgens ETL-specificatie."

// PatientIdentificatienummer: eerst een PatientSan-reference bepalen.
* patient -> "DBC;Identificatienummer" "Reference naar PatientSan op basis van patiëntidentifier."

// ZorgType: inhoudelijke overeenkomst met EpisodeOfCare.type verifiëren.
* type.coding.system -> "DBC;ZorgTypeCodeSysteem"
* type.coding.code -> "DBC;ZorgTypeCode"
* type.coding.display -> "DBC;ZorgTypeOmschrijving"

// Periode: datumprecisie behouden; geen tijdstip verzinnen.
* period.start -> "DBC;DBCOpeningsDatum" "Alleen indien gelijk aan begin van de zorgepisode."
* period.end -> "DBC;DBCSluitingsDatum" "Alleen indien gelijk aan einde van de zorgepisode."

// Nog uit te werken:
// EpisodeOfCare.status: verplicht, geen directe bronmapping.
// Diagnose en SpecialismeDiagnose: eventueel Condition.code,
// via EpisodeOfCare.diagnosis.condition (Reference(Condition)).
// Geen directe mapping zonder passende Condition-resource.
// ZorgProduct, Gevuld, Geldig en UitvoerderSpecialisme:
// geen semantisch bevestigde EpisodeOfCare-mapping.
// ZorgtrajectNummer en Verwijzer: momenteel uitgeschakeld in LM.
