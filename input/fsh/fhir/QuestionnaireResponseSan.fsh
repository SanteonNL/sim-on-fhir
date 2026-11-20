Profile: QuestionnaireResponseSan
Parent: QuestionnaireResponse
Id: questionnaireResponse-san
Title: "QuestionnaireResponse (SAN)"
Description: "Generiek Santeon-profiel voor ingevulde vragenlijsten (SIMVragenlijstAntwoord), gebaseerd op FHIR R4 QuestionnaireResponse (er is geen Nictiz GBB- of HL7 Europe-profiel). Records met hetzelfde VragenlijstAntwoordID vormen samen één QuestionnaireResponse. Datasetprofielen sluiten de elementen die zij niet gebruiken."

// Opgenomen
* identifier 1..1 MS
* identifier.value 1..1
* questionnaire 0..1 MS
* status 1..1 MS
* subject 1..1 MS
* subject only Reference(PatientSan)
* authored 0..1 MS
* item 0..* MS
* item.linkId 1..1 MS
* item.answer 0..* MS
* item.answer.value[x] only Quantity or Coding or dateTime

// Uitgesloten
* implicitRules 0..0
* text 0..0
* contained 0..0
* modifierExtension 0..0


Mapping: QuestionnaireResponseSanFromSIMCSV
Id: sim
Title: "Santeon Informatiemodel (SIM CSV)"
Description: "Informatieve mapping van FHIR QuestionnaireResponse naar SIM CSV-bronvelden."
Source: QuestionnaireResponseSan
Target: "urn:santeon:sim:csv"

* identifier.system -> "VragenlijstAntwoord;VragenlijstAntwoordID" "Systeemdeel van de samengestelde identifier."
* identifier.value -> "VragenlijstAntwoord;VragenlijstAntwoordID" "Waardedeel van de samengestelde identifier; hash."
* subject -> "VragenlijstAntwoord;Identificatienummer" "Patient-reference op basis van patiëntidentifier."
* authored -> "VragenlijstAntwoord;InvulDatumTijd"
* questionnaire -> "VragenlijstAntwoord;QuestionnaireResponseQuestionnaire" "Canonical-verwijzing naar de Questionnaire."
* item.linkId -> "VragenlijstAntwoord;QuestionnaireResponseItemLinkId" "Verwijzing naar het item in de Questionnaire."
* item.answer.valueQuantity.value -> "VragenlijstAntwoord;AntwoordWaarde"
* item.answer.valueQuantity.system -> "VragenlijstAntwoord;AntwoordWaardeEenheidSysteem"
* item.answer.valueQuantity.code -> "VragenlijstAntwoord;AntwoordWaardeEenheid"
* item.answer.valueQuantity.comparator -> "VragenlijstAntwoord;AntwoordWaardeOperator"
* item.answer.valueCoding.system -> "VragenlijstAntwoord;AntwoordCodeSysteem"
* item.answer.valueCoding.code -> "VragenlijstAntwoord;AntwoordCode"
* item.answer.valueCoding.display -> "VragenlijstAntwoord;AntwoordCodeOmschrijving"
* item.answer.valueDateTime -> "VragenlijstAntwoord;AntwoordDatumTijd"
