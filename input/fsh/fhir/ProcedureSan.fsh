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

// SIMVerrichting.VerrichtingID
* identifier 1..1
* identifier.use 0..0
* identifier.value 1..1
// identifier.system blijft toegestaan, omdat het SIM
// identifier_system|identifier_value ondersteunt.

// FHIR R4: Procedure.status is verplicht (1..1).
// Huidige SIMVerrichting bevat geen statusveld.
* status 1..1
* status ^short = "Status van de verrichting"
* status ^comment = "Verplicht vanuit FHIR R4. De huidige SIMVerrichting bevat geen statusveld 🔴. Herkomst en eventuele afleiding vanuit de verrichtingsregistratie nader te bepalen."


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
* category ^short = "Santeon zorgprofielklasse"
* category ^comment = "Gebaseerd op de lokale Santeon VerrichtingCategorieCodelijst. Deze classificatie is niet één-op-één equivalent aan de Nictiz GBB VerrichtingCategorieCodelijst 🟠."

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
* code.coding.system -> "Verrichting;VerrichtingTypeCodeSnomedCTSyst" "SNOMED CT-codering."
* code.coding.code -> "Verrichting;VerrichtingTypeCodeSnomedCT" "SNOMED CT-codering."
* code.coding.display -> "Verrichting;VerrichtingTypeOmschrijvingSnomedCT" "SNOMED CT-codering."

// VerrichtingType: DHD
* code.coding.system -> "Verrichting;VerrichtingTypeCodeDHDSysteem" "DHD-codering."
* code.coding.code -> "Verrichting;VerrichtingTypeCodeDHD" "DHD-codering."
* code.coding.display -> "Verrichting;VerrichtingTypeOmschrijvingDHD" "DHD-codering."

// VerrichtingCategorie
* category.coding.system -> "Verrichting;VerrichtingCategorieSysteem"
* category.coding.code -> "Verrichting;VerrichtingCategorieCode"
* category.coding.display -> "Verrichting;VerrichtingCategorieOmschrijving"

// Datum/tijd van de verrichting: alternatieve FHIR-representaties
* performedDateTime -> "Verrichting;VerrichtingStartDatum" "Momentane verrichting; dateshift."
* performedPeriod.start -> "Verrichting;VerrichtingStartDatum" "Verrichting met tijdsduur; dateshift."
// * performedPeriod.end -> "Verrichting;VerrichtingEindDatum" "Nog niet actief in het SIM LM."

// Nog zonder directe FHIR-mapping:
// VerrichtingAantal, OperatieID, SubtrajectNummer,
// AanvragerSpecialisme, UitvoerderSpecialisme.
// Procedure.status: verplicht, maar geen expliciete SIM-bronkolom.


Mapping: ProcedureSanToNlCore
Id: ProcedureSanToNlCore
Title: "ProcedureSan naar Nictiz nl-core-Procedure"
Description: "FHIR-profielvergelijking tussen ProcedureSan en Nictiz nl-core-Procedure (GBB 1.0.0-alpha.2), binnen de huidige IBD-scope."
Source: ProcedureSan
Target: "http://ig.nictiz.nl/gbb/StructureDefinition/nl-core-Procedure"

// Documentatie:
// https://ig.nictiz.nl/gbb/1.0.0-alpha.2/nl/StructureDefinition-nl-core-Procedure.html

// IBD - Identificatie
* identifier -> "Procedure.identifier" "Santeon 1..1; nl-core 0..*. Santeon strenger."
* identifier.system -> "Procedure.identifier.system" "Identificatiesysteem behouden."
* identifier.value -> "Procedure.identifier.value" "Santeon vereist een waarde (1..1)."
* identifier.use -> "Procedure.identifier.use" "Santeon 0..0; nl-core staat dit toe."

// IBD - Status
* status -> "Procedure.status" "Beide 1..1, required FHIR EventStatus. Herkomst vanuit SIM nog te bepalen."

// IBD - VerrichtingType
* code -> "Procedure.code" "Santeon 0..1 versus nl-core 1..1. Nictiz preferred SNOMED ValueSet; Santeon NZa, DHD en/of SNOMED CT."

* code.coding -> "Procedure.code.coding" "Santeon vereist minimaal een Coding wanneer code aanwezig is. Meerdere coderingen uitsluitend bij semantische equivalentie."

// IBD - VerrichtingCategorie
* category -> "Procedure.category" "Beide 0..1. Santeon lokale zorgprofielklassen; Nictiz preferred SNOMED ValueSet. Geen directe één-op-één-equivalentie."

// IBD - Patient
* subject -> "Procedure.subject" "Beide 1..1. Santeon Reference(PatientSan); nl-core ondersteunt onder andere nl-core-Patient."

// IBD - Contact
* encounter -> "Procedure.encounter" "Santeon Reference(EncounterSan). Doelprofiel en eventuele aanvullende constraints verschillen."

// IBD - Verrichtingdatum
* performed[x] -> "Procedure.performed[x]" "Santeon beperkt tot dateTime of Period; nl-core ondersteunt daarnaast andere FHIR R4-varianten."

* performedDateTime -> "Procedure.performedDateTime" "Moment van verrichting; Nictiz ondersteunt deze representatie."

* performedPeriod.start -> "Procedure.performedPeriod.start" "Startmoment van verrichting."
* performedPeriod.end -> "Procedure.performedPeriod.end" "Eindmoment; nog niet actief in huidige SIM-scope."

// Overige relevante delta's:
//
// Procedure.extension:method:
// Nictiz ondersteunt verrichtingsmethode.
// Geen directe SIM-mapping vastgesteld.
//
// Procedure.extension:extensionProcedureRecorded:
// Nictiz ondersteunt datum van eerste registratie.
// Niet automatisch gelijk aan VerrichtingStartDatum.
//
// Procedure.partOf:
// Santeon sluit momenteel bovenliggende
// verrichtingsrelaties uit.
//
// Procedure.performer:
// Santeon laat performer beschikbaar.
// Exacte mapping van UitvoerderSpecialisme
// naar PractitionerRole nog te beoordelen.
//
// Procedure.location, reasonCode, reasonReference,
// bodySite, outcome, complication en andere
// standaardvelden:
// Momenteel deels uitgesloten in ProcedureSan.
//
// MustSupport en obligations:
// Nictiz specificeert aanvullende verplichtingen
// voor uitwisselende en verwerkende systemen.
// Nog niet overgenomen in ProcedureSan.
//
// Deze mapping documenteert de verschillen.
// Er wordt geen automatische conformiteit afgedwongen.
