Logical: SIMVragenlijstAntwoord
Parent: Base
Id: SIMVragenlijstAntwoord
Title: "Vragenlijstantwoord (model)"
Description: "Generiek logisch model voor een antwoord op een vraag uit een ingevulde vragenlijst binnen het Santeon Informatiemodel."

// IBD-scope
* VragenlijstAntwoordID 1..1 Identifier "Uniek ID (key) binnen patiënt per ingevulde vragenlijst"
* PatientIdentificatienummer 1..1 Identifier "Identificatienummer van de patiënt"
* InvulDatumTijd 0..1 dateTime "Datum en eventueel tijd waarop de antwoorden zijn verzameld"
* Vragenlijst 0..1 Coding "Code van de vragenlijst waartoe het antwoord behoort"
* Vraag 0..1 Coding "Code van de vraag waarop het antwoord is gegeven"
* AntwoordWaarde 0..1 Quantity "Numeriek antwoord op de vraag, met eventueel eenheid (UCUM) en vergelijkingsoperator"
* AntwoordCode 0..1 Coding "Gecodeerd antwoord op de vraag"
* AntwoordDatumTijd 0..1 dateTime "Datum en eventueel tijd als antwoord op de vraag"
// Eén record bevat één antwoord op één vraag. Een antwoord heeft maximaal één antwoordtype (AntwoordWaarde, AntwoordCode of AntwoordDatumTijd).

// Buiten IBD-scope - nog niet meegenomen
// * ContactID 0..1 Identifier "Uniek ID (key) binnen patiënt per contact"
// * QuestionnaireResponseQuestionnaire 0..1 uri "Verwijzing naar de Questionnaire waarop de antwoorden betrekking hebben"
// * QuestionnaireResponseItemLinkId 0..1 string "Verwijzing naar het item in de Questionnaire"

// Terminologie
// Nog geen actieve bindingen. Te beoordelen:
// Vragenlijst: codestelsel uit SIM-bron vaststellen.
// Vraag: in de IBD-indicatoren Santeon-codes IC_000253 t/m IC_000257.
// AntwoordCode: ValueSet afhankelijk van Vraag.



// ============================================
// MAPPING 1: Herkomst - SIM CSV
// ============================================

Mapping: SIMVragenlijstAntwoordFromSIMCSV
Id: SIMVragenlijstAntwoordFromSIMCSV
Title: "SIM CSV"
Description: "Herleidbaarheid van SIMVragenlijstAntwoord-elementen naar de oorspronkelijke SIM CSV-attributen."
Source: SIMVragenlijstAntwoord
Target: "urn:santeon:sim:csv"

// IBD-scope
* VragenlijstAntwoordID -> "VragenlijstAntwoord;VragenlijstAntwoordID"
* PatientIdentificatienummer -> "VragenlijstAntwoord;Identificatienummer"
* InvulDatumTijd -> "VragenlijstAntwoord;InvulDatumTijd"
* Vragenlijst.system -> "VragenlijstAntwoord;QuestionnaireCodeCodingSystem"
* Vragenlijst.code -> "VragenlijstAntwoord;QuestionnaireCodeCodingCode"
* Vragenlijst.display -> "VragenlijstAntwoord;QuestionnaireCodeCodingDisplay"
* Vraag.system -> "VragenlijstAntwoord;QuestionnaireItemCodeCodingSystem"
* Vraag.code -> "VragenlijstAntwoord;QuestionnaireItemCodeCodingCode"
* Vraag.display -> "VragenlijstAntwoord;QuestionnaireItemCodeCodingDisplay"
* AntwoordWaarde.value -> "VragenlijstAntwoord;AntwoordWaarde"
* AntwoordWaarde.system -> "VragenlijstAntwoord;AntwoordWaardeEenheidSysteem"
* AntwoordWaarde.code -> "VragenlijstAntwoord;AntwoordWaardeEenheid"
* AntwoordWaarde.comparator -> "VragenlijstAntwoord;AntwoordWaardeOperator"
* AntwoordCode.system -> "VragenlijstAntwoord;AntwoordCodeSysteem"
* AntwoordCode.code -> "VragenlijstAntwoord;AntwoordCode"
* AntwoordCode.display -> "VragenlijstAntwoord;AntwoordCodeOmschrijving"
* AntwoordDatumTijd -> "VragenlijstAntwoord;AntwoordDatumTijd"

// Buiten IBD-scope - nog niet meegenomen
// * ContactID -> "VragenlijstAntwoord;ContactID"
// * QuestionnaireResponseQuestionnaire -> "VragenlijstAntwoord;QuestionnaireResponseQuestionnaire"
// * QuestionnaireResponseItemLinkId -> "VragenlijstAntwoord;QuestionnaireResponseItemLinkId"


// ============================================
// MAPPING 2: Semantische delta-analyse - SIMVragenlijstAntwoord naar Nictiz GBB-afsprakenmodel
// ============================================

// Geen mapping: Nictiz GBB (referentieversie 1.0.0-alpha.2) bevat geen
// overeenkomstig afsprakenmodel voor vragenlijstantwoorden.


// ============================================
// MAPPING 3: Technische vertaling, harmonisatie en conversie - SIMVragenlijstAntwoord naar QuestionnaireResponseSan
// De omgekeerde mapping (QuestionnaireResponseSan naar SIMVragenlijstAntwoord) wordt automatisch gegenereerd.
// ============================================

Mapping: SIMVragenlijstAntwoordToFHIR
Id: SIMVragenlijstAntwoordToFHIR
Title: "FHIR-mapping naar QuestionnaireResponseSan"
Description: "Mapping van SIMVragenlijstAntwoord-elementen naar het Santeon FHIR-profiel QuestionnaireResponseSan, inclusief harmonisatie met FHIR-standaarden en toelichting op benodigde conversies."
Source: SIMVragenlijstAntwoord
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/questionnaireResponse-san"

// IBD-scope
* VragenlijstAntwoordID -> "QuestionnaireResponse.identifier" "Direct: samengestelde identifier (system|value); het waardedeel wordt gehasht volgens de ETL-specificatie."
* PatientIdentificatienummer -> "QuestionnaireResponse.subject" "Structuurconversie: Patient-reference op basis van identificatienummer; SIM benoemt daarnaast QuestionnaireResponse.source."
* InvulDatumTijd -> "QuestionnaireResponse.authored" "Direct: SIM dateTime wordt overgenomen als FHIR dateTime."
* AntwoordWaarde -> "QuestionnaireResponse.item.answer.valueQuantity" "Structuurconversie: één SIM-record wordt één item met één answer; SIM Quantity wordt overgenomen als FHIR Quantity."
* AntwoordCode -> "QuestionnaireResponse.item.answer.valueCoding" "Structuurconversie: één SIM-record wordt één item met één answer; SIM Coding wordt overgenomen als FHIR Coding."
* AntwoordDatumTijd -> "QuestionnaireResponse.item.answer.valueDateTime" "Structuurconversie: één SIM-record wordt één item met één answer; SIM dateTime wordt overgenomen als FHIR dateTime."


// ============================================
// Status en openstaande controles
// ============================================

// Status: In progress
// TODO: QuestionnaireResponse.status is verplicht in FHIR R4 en ontbreekt in de SIM-bron.
// TODO: Vragenlijst: de code hoort bij Questionnaire.code. QuestionnaireResponse.questionnaire is een canonical-verwijzing; vaststellen hoe de Questionnaire wordt geïdentificeerd of aangemaakt.
// TODO: Vraag: de code hoort bij Questionnaire.item.code. QuestionnaireResponse.item.linkId is verplicht; vaststellen hoe linkId uit de vraagcode wordt afgeleid. De IBD-indicatoren selecteren op de vraagcode (IC_000253 t/m IC_000257); daarvoor is het SIM-model Vragenlijst (Questionnaire) of een extensie op item nodig.
// TODO: Groepering: SIM levert één record per antwoord. Vaststellen hoe records met hetzelfde VragenlijstAntwoordID tot één QuestionnaireResponse worden samengevoegd.
// TODO: Terminologie voor Vragenlijst, Vraag en AntwoordCode beoordelen (zie Terminologie).
