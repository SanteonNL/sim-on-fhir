Profile: ProcedureSan
Parent: Procedure
Id: procedure-san
Title: "Procedure (SAN)"
Description: """
Generiek Santeon FHIR R4-profiel voor verrichtingen.

Gebaseerd op het Santeon Informatiemodel (SIMVerrichting).

De-identificatie wordt uitgevoerd volgens afzonderlijke
transformatiespecificaties en is geen inherente
eigenschap van dit profiel.
"""

// Meta overhead: bestaande profielkeuze; opnieuw beoordelen
// zodra meta.profile of meta.security vereist is.
* meta 0..0
* implicitRules 0..0
* language 0..0
* text 0..0
* contained 0..0
* extension 0..0
* modifierExtension 0..0

// SIMVerrichting.VerrichtingID
* identifier 1..1
* identifier.use 0..0
* identifier.value 1..1
// identifier.system blijft toegestaan, omdat het SIM
// identifier_system|identifier_value ondersteunt.

// Procedure.status is verplicht vanuit FHIR R4 (1..1).
// De huidige SIMVerrichting bevat geen statusveld.
// TODO: herkomst of gevalideerde ETL2-afleidingsregel vaststellen.

// SIMVerrichting.VerrichtingType
// Optioneel conform SIM; afzonderlijke NZa-, SNOMED CT-
// en DHD-coderingen alleen combineren bij semantische overeenkomst.
* code 0..1
* code.coding 1..*
* code.coding.system 1..1
* code.coding.code 1..1
* code.coding.display 0..1

// SIMVerrichting.VerrichtingCategorie
* category 0..1

// SIMVerrichting.PatientIdentificatienummer
* subject 1..1
* subject only Reference(PatientSan)

// SIMVerrichting.ContactID
* encounter 0..1
* encounter only Reference(EncounterSan)

// SIMVerrichting.VerrichtingStartDatum
// Een afzonderlijk tijdstip -> performedDateTime.
// Een verrichting met tijdsduur -> performedPeriod.
// Startdatum is nullable in het SIM; dus niet verplicht stellen.
* performed[x] only dateTime or Period
* performedPeriod.start 0..1
* performedPeriod.end 0..1
// SIMVerrichting.VerrichtingEindDatum staat momenteel uit.

// Overige elementen: bestaande, beperkte scope van ProcedureSan.
// Voor een breder generiek profiel heroverwegen of 0..0 nodig is.
* instantiatesCanonical 0..0
* instantiatesUri 0..0
* partOf 0..0
* statusReason 0..0
* recorder 0..0
* asserter 0..0
* location 0..0
* reasonCode 0..0
* reasonReference 0..0
* bodySite 0..0
* outcome 0..0
* report 0..0
* complication 0..0
* complicationDetail 0..0
* followUp 0..0
* note 0..0
* focalDevice 0..0
* usedReference 0..0
* usedCode 0..0
// performer blijft beschikbaar voor eventuele latere mapping
// van UitvoerderSpecialisme, mits PractitionerRole bekend is.


Mapping: ProcedureSanFromSIMCSV
Id: sim
Title: "Santeon Informatiemodel (SIM CSV)"
Description: "Informatieve mapping van het FHIR-profiel naar SIM CSV-bronvelden."
Source: ProcedureSan
Target: "urn:santeon:sim:csv"

// VerrichtingID: samengestelde bronidentifier
* identifier.system -> "Verrichting;VerrichtingID" "Systeemdeel van identifier_system|identifier_value."
* identifier.value -> "Verrichting;VerrichtingID" "Waardedeel; hashed."

// PatientIdentificatienummer: eerst een PatientSan-reference bepalen
* subject -> "Verrichting;Identificatienummer" "Reference naar gepseudonimiseerde PatientSan op basis van patiëntidentifier."

// ContactID: alleen een Encounter-reference als contact te identificeren is
* encounter -> "Verrichting;ContactID" "Reference naar Encounter op basis van ContactID."

// VerrichtingType: NZa
* code.coding.system -> "Verrichting;VerrichtingTypeCodeNZaSysteem" "NZa-codering."
* code.coding.code -> "Verrichting;VerrichtingTypeCodeNZa" "NZa-codering."
* code.coding.display -> "Verrichting;VerrichtingTypeOmschrijvingNZa" "NZa-codering."

// VerrichtingType: SNOMED CT
* code.coding.system -> "Verrichting;VerrichtingTypeCodeSnomedCTSysteem" "SNOMED CT-codering."
* code.coding.code -> "Verrichting;VerrichtingTypeCodeSnomedCT" "SNOMED CT-codering."
* code.coding.display -> "Verrichting;VerrichtingTypeOmschrijvingSnomedCT" "SNOMED CT-codering."

// VerrichtingType: DHD
* code.coding.system -> "Verrichting;VerrichtingTypeCodeDHDSysteem" "DHD-codering."
* code.coding.code -> "Verrichting;VerrichtingTypeCodeDHD" "DHD-codering."
* code.coding.display -> "Verrichting;VerrichtingTypeOmschrijvingDHD" "DHD-codering."

// VerrichtingCategorie: bron bevestigt alleen de codekolom
* category.coding.code -> "Verrichting;VerrichtingCategorieCode" "CodeSystem afzonderlijk verifiëren."

// Datum/tijd van de verrichting: alternatieve FHIR-representaties
* performedDateTime -> "Verrichting;VerrichtingStartDatum" "Momentane verrichting; dateshift."
* performedPeriod.start -> "Verrichting;VerrichtingStartDatum" "Verrichting met tijdsduur; dateshift."
// * performedPeriod.end -> "Verrichting;VerrichtingEindDatum" "Nog niet actief in het SIM LM."

// Nog zonder directe FHIR-mapping:
// VerrichtingAantal, OperatieID, SubtrajectNummer,
// AanvragerSpecialisme, UitvoerderSpecialisme.
// Procedure.status: verplicht, maar geen expliciete SIM-bronkolom.
