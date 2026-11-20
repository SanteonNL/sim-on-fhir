Profile: ProcedureSan
Parent: http://ig.nictiz.nl/gbb/StructureDefinition/nl-core-Procedure
Id: procedure-san
Title: "Procedure (SAN)"
Description: "Generiek Santeon-profiel voor verrichtingen (SIMVerrichting), gebaseerd op Nictiz nl-core-Procedure. Datasetprofielen sluiten de elementen die zij niet gebruiken."

// Opgenomen
* identifier 1..1 MS
* identifier.value 1..1
* status 1..1 MS
* code 1..1 MS
* code.coding 1..*
* code.coding.system 1..1
* code.coding.code 1..1
* category 0..1 MS
* subject 1..1 MS
* subject only Reference(PatientSan)
* encounter 0..1 MS
* encounter only Reference(EncounterSan)
* performed[x] 0..1 MS
* performed[x] only dateTime or Period

// Uitgesloten
* implicitRules 0..0
* text 0..0
* contained 0..0
* modifierExtension 0..0


Mapping: ProcedureSanFromSIMCSV
Id: sim
Title: "Santeon Informatiemodel (SIM CSV)"
Description: "Informatieve mapping van het FHIR-profiel naar SIM CSV-bronvelden."
Source: ProcedureSan
Target: "urn:santeon:sim:csv"

* identifier.system -> "Verrichting;VerrichtingID" "Systeemdeel van identifier_system|identifier_value."
* identifier.value -> "Verrichting;VerrichtingID" "Waardedeel; hashed."
* subject -> "Verrichting;Identificatienummer" "Reference naar gepseudonimiseerde PatientSan op basis van patiëntidentifier."
* encounter -> "Verrichting;ContactID" "Reference naar Encounter op basis van ContactID."
* code.coding.system -> "Verrichting;VerrichtingTypeCodeNZaSysteem" "NZa-codering."
* code.coding.code -> "Verrichting;VerrichtingTypeCodeNZa" "NZa-codering."
* code.coding.display -> "Verrichting;VerrichtingTypeOmschrijvingNZa" "NZa-codering."
* code.coding.system -> "Verrichting;VerrichtingTypeCodeSnomedCTSyst" "SNOMED CT-codering."
* code.coding.code -> "Verrichting;VerrichtingTypeCodeSnomedCT" "SNOMED CT-codering."
* code.coding.display -> "Verrichting;VerrichtingTypeOmschrijvingSnomedCT" "SNOMED CT-codering."
* code.coding.system -> "Verrichting;VerrichtingTypeCodeDHDSysteem" "DHD-codering."
* code.coding.code -> "Verrichting;VerrichtingTypeCodeDHD" "DHD-codering."
* code.coding.display -> "Verrichting;VerrichtingTypeOmschrijvingDHD" "DHD-codering."
* category.coding.system -> "Verrichting;VerrichtingCategorieSysteem"
* category.coding.code -> "Verrichting;VerrichtingCategorieCode"
* category.coding.display -> "Verrichting;VerrichtingCategorieOmschrijving"
* performedDateTime -> "Verrichting;VerrichtingStartDatum" "Momentane verrichting; dateshift."
* performedPeriod.start -> "Verrichting;VerrichtingStartDatum" "Verrichting met tijdsduur; dateshift."
