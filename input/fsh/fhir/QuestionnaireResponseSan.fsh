
Profile: QuestionnaireResponseSan
Parent: QuestionnaireResponse
Id: questionnaireResponse-san
Title: "QuestionnaireResponse (SAN)"
Description: """
Generiek Santeon FHIR R4-profiel voor ingevulde vragenlijsten.

Gebaseerd op het Santeon Informatiemodel (SIMVragenlijstAntwoord).
Het SIM levert één record per antwoord; records met hetzelfde
VragenlijstAntwoordID vormen samen één QuestionnaireResponse.

De-identificatie wordt uitgevoerd volgens afzonderlijke
transformatiespecificaties en is geen inherente
eigenschap van dit profiel.
"""

// Identifier (SIMVragenlijstAntwoord.VragenlijstAntwoordID)
* identifier 1..1
* identifier.use 0..0
* identifier.value 1..1
// identifier.system blijft toegestaan, omdat het SIM
// identifier_system|identifier_value ondersteunt.

// QuestionnaireResponse.status is verplicht in FHIR R4 (1..1).
// De SIM-bron bevat geen status.
// TODO: herkomst of gevalideerde afleidingsregel vaststellen.

// Vragenlijst
// De code van de vragenlijst hoort bij Questionnaire.code;
// hier is alleen een canonical-verwijzing mogelijk.
* questionnaire 0..1

// PatientIdentificatienummer
* subject 1..1
* subject only Reference(PatientSan)

// InvulDatumTijd
* authored 0..1

// Vraag en antwoord: één item per SIM-record.
// item.linkId is verplicht in FHIR R4 (1..1).
// FHIR answer.value[x] staat per antwoord één type toe.
* item 0..*
* item.answer.value[x] only Quantity or Coding or dateTime

// Overige FHIR-elementen blijven beschikbaar.
// Toepassingsspecifieke beperkingen kunnen
// in een afgeleid profiel worden opgenomen.


Mapping: QuestionnaireResponseSanFromSIMCSV
Id: sim
Title: "Santeon Informatiemodel (SIM CSV)"
Description: "Informatieve mapping van FHIR QuestionnaireResponse naar SIM CSV-bronvelden."
Source: QuestionnaireResponseSan
Target: "urn:santeon:sim:csv"

// Identificatie
* identifier.system -> "VragenlijstAntwoord;VragenlijstAntwoordID" "Systeemdeel van de samengestelde identifier."
* identifier.value -> "VragenlijstAntwoord;VragenlijstAntwoordID" "Waardedeel van de samengestelde identifier; hash."

* subject -> "VragenlijstAntwoord;Identificatienummer" "Patient-reference op basis van patiëntidentifier."

* authored -> "VragenlijstAntwoord;InvulDatumTijd"

// Vragenlijst en vraag
// Deze twee SIM-kolommen staan uitgeschakeld in het SIM LM (buiten IBD-scope).
* questionnaire -> "VragenlijstAntwoord;QuestionnaireResponseQuestionnaire" "Canonical-verwijzing naar de Questionnaire."
* item.linkId -> "VragenlijstAntwoord;QuestionnaireResponseItemLinkId" "Verwijzing naar het item in de Questionnaire."

// Numeriek antwoord
* item.answer.valueQuantity.value -> "VragenlijstAntwoord;AntwoordWaarde"
* item.answer.valueQuantity.system -> "VragenlijstAntwoord;AntwoordWaardeEenheidSysteem"
* item.answer.valueQuantity.code -> "VragenlijstAntwoord;AntwoordWaardeEenheid"
* item.answer.valueQuantity.comparator -> "VragenlijstAntwoord;AntwoordWaardeOperator"

// Gecodeerd antwoord
* item.answer.valueCoding.system -> "VragenlijstAntwoord;AntwoordCodeSysteem"
* item.answer.valueCoding.code -> "VragenlijstAntwoord;AntwoordCode"
* item.answer.valueCoding.display -> "VragenlijstAntwoord;AntwoordCodeOmschrijving"

// Datum/tijd als antwoord
* item.answer.valueDateTime -> "VragenlijstAntwoord;AntwoordDatumTijd"

// Nog uit te werken:
// QuestionnaireResponse.status: verplicht, geen SIM-bronkolom.
// Patiënt: het SIM noemt als pad QuestionnaireResponse.source en in de
// titel QuestionnaireResponse.subject; dit profiel gebruikt subject.
// Vragenlijst (QuestionnaireCodeCoding*) en Vraag
// (QuestionnaireItemCodeCoding*): het SIM documenteert deze kolommen als
// Questionnaire.code en Questionnaire.item.code. Het SIM kent daarvoor
// een apart model Vragenlijst (Questionnaire), gekoppeld via
// questionnaire en item.linkId. De IBD-indicatoren selecteren op de
// vraagcode (IC_000253 t/m IC_000257); daarvoor is dus het model
// Vragenlijst of een extensie op item nodig.
// ContactID: QuestionnaireResponse.encounter (uitgeschakeld in LM).
