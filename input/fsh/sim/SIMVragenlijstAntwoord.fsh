Logical: SIMVragenlijstAntwoord
Parent: Base
// Parent: GBB - nog te beoordelen
Id: SIMVragenlijstAntwoord
Title: "Vragenlijstantwoord (model)"
Description: "Generiek logisch model voor een antwoord op een vraag uit een ingevulde vragenlijst binnen het Santeon Informatiemodel."

// IBD
* VragenlijstAntwoordID 1..1 Identifier "Uniek ID (key) binnen patiënt per ingevulde vragenlijst"
* PatientIdentificatienummer 1..1 Identifier "Identificatienummer van de patiënt"

* InvulDatumTijd 0..1 dateTime "Datum en eventueel tijd waarop de antwoorden zijn verzameld"

* Vragenlijst 0..1 Coding "Code van de vragenlijst waartoe het antwoord behoort"
* Vraag 0..1 Coding "Code van de vraag waarop het antwoord is gegeven"

* AntwoordWaarde 0..1 Quantity "Numeriek antwoord op de vraag, met eventueel eenheid (UCUM) en vergelijkingsoperator"
* AntwoordCode 0..1 Coding "Gecodeerd antwoord op de vraag"
* AntwoordDatumTijd 0..1 dateTime "Datum en eventueel tijd als antwoord op de vraag"

// Eén record bevat één antwoord op één vraag.
// Een antwoord heeft maximaal één antwoordtype.
// FHIR: QuestionnaireResponse.item.answer.value[x] met Quantity,
// Coding of dateTime.

// Overig van model VragenlijstAntwoord
// * ContactID 0..1 Identifier "Uniek ID (key) binnen patiënt per contact"
// * QuestionnaireResponseQuestionnaire 0..1 uri "Verwijzing naar de Questionnaire waarop de antwoorden betrekking hebben"
// * QuestionnaireResponseItemLinkId 0..1 string "Verwijzing naar het item in de Questionnaire"

// Terminologie nog te beoordelen:
// Vragenlijst: codestelsel uit SIM-bron vaststellen
// Vraag: in de IBD-indicatoren Santeon-codes IC_000253 t/m IC_000257
// AntwoordCode: ValueSet afhankelijk van Vraag


// Mapping vanuit SIM CSV
Mapping: SIMVragenlijstAntwoordFromSIMCSV
Id: SIMVragenlijstAntwoordFromSIMCSV
Title: "SIM CSV"
Source: SIMVragenlijstAntwoord
Target: "urn:santeon:sim:csv"

// Identificatie
* VragenlijstAntwoordID -> "VragenlijstAntwoord;VragenlijstAntwoordID"
* PatientIdentificatienummer -> "VragenlijstAntwoord;Identificatienummer"

* InvulDatumTijd -> "VragenlijstAntwoord;InvulDatumTijd"

// Vragenlijst
* Vragenlijst.system -> "VragenlijstAntwoord;QuestionnaireCodeCodingSystem"
* Vragenlijst.code -> "VragenlijstAntwoord;QuestionnaireCodeCodingCode"
* Vragenlijst.display -> "VragenlijstAntwoord;QuestionnaireCodeCodingDisplay"

// Vraag
* Vraag.system -> "VragenlijstAntwoord;QuestionnaireItemCodeCodingSystem"
* Vraag.code -> "VragenlijstAntwoord;QuestionnaireItemCodeCodingCode"
* Vraag.display -> "VragenlijstAntwoord;QuestionnaireItemCodeCodingDisplay"

// Numeriek antwoord
* AntwoordWaarde.value -> "VragenlijstAntwoord;AntwoordWaarde"
* AntwoordWaarde.system -> "VragenlijstAntwoord;AntwoordWaardeEenheidSysteem"
* AntwoordWaarde.code -> "VragenlijstAntwoord;AntwoordWaardeEenheid"
* AntwoordWaarde.comparator -> "VragenlijstAntwoord;AntwoordWaardeOperator"

// Gecodeerd antwoord
* AntwoordCode.system -> "VragenlijstAntwoord;AntwoordCodeSysteem"
* AntwoordCode.code -> "VragenlijstAntwoord;AntwoordCode"
* AntwoordCode.display -> "VragenlijstAntwoord;AntwoordCodeOmschrijving"

// Datum/tijd als antwoord
* AntwoordDatumTijd -> "VragenlijstAntwoord;AntwoordDatumTijd"

// Overig van model VragenlijstAntwoord
// * ContactID -> "VragenlijstAntwoord;ContactID"
// * QuestionnaireResponseQuestionnaire -> "VragenlijstAntwoord;QuestionnaireResponseQuestionnaire"
// * QuestionnaireResponseItemLinkId -> "VragenlijstAntwoord;QuestionnaireResponseItemLinkId"


// Mapping naar FHIR

Mapping: SIMVragenlijstAntwoordToFHIR
Id: SIMVragenlijstAntwoordToFHIR
Title: "SIMVragenlijstAntwoord naar FHIR"
Source: SIMVragenlijstAntwoord
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/questionnaireResponse-san"

// Identificatie
* VragenlijstAntwoordID -> "QuestionnaireResponse.identifier"
* PatientIdentificatienummer -> "QuestionnaireResponse.subject" "Patient-reference op basis van identificatienummer; SIM benoemt daarnaast QuestionnaireResponse.source."

* InvulDatumTijd -> "QuestionnaireResponse.authored"

// Antwoord
* AntwoordWaarde -> "QuestionnaireResponse.item.answer.valueQuantity"
* AntwoordCode -> "QuestionnaireResponse.item.answer.valueCoding"
* AntwoordDatumTijd -> "QuestionnaireResponse.item.answer.valueDateTime"

// Nog uit te werken:
//
// QuestionnaireResponse.status: verplicht in FHIR R4; ontbreekt in de SIM-bron.
//
// Vragenlijst:
// De code hoort bij Questionnaire.code. QuestionnaireResponse.questionnaire
// is een canonical-verwijzing; vaststellen hoe de Questionnaire wordt
// geïdentificeerd of aangemaakt.
//
// Vraag:
// De code hoort bij Questionnaire.item.code. QuestionnaireResponse.item.linkId
// is verplicht; vaststellen hoe linkId uit de vraagcode wordt afgeleid.
//
// Groepering:
// SIM levert één record per antwoord. Vaststellen hoe records met hetzelfde
// VragenlijstAntwoordID tot één QuestionnaireResponse worden samengevoegd.

// GBB: nog te beoordelen of Nictiz GBB een overeenkomstig
// Logical Model voor vragenlijstantwoorden biedt.
