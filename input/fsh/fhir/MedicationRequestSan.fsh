Profile: MedicationRequestSan
Parent: http://hl7.eu/fhir/base/StructureDefinition/medicationRequest-eu-core
Id: medicationRequest-san
Title: "MedicationRequest (SAN)"
Description: "Generiek Santeon-profiel voor medicatieafspraken (SIMMedicatieAfspraak), gebaseerd op HL7 Europe medicationRequest-eu-core (er is geen Nictiz GBB-profiel). Datasetprofielen sluiten de elementen die zij niet gebruiken."

// Opgenomen
* identifier 1..1 MS
* identifier.value 1..1
* status 1..1 MS
* intent 1..1 MS
* medication[x] 1..1 MS
* subject 1..1 MS
* subject only Reference(PatientSan)
* dosageInstruction 0..* MS
* dosageInstruction.route 0..1 MS
* dosageInstruction.timing 0..1 MS

// Uitgesloten
* implicitRules 0..0
* text 0..0
* contained 0..0
* modifierExtension 0..0


Mapping: MedicationRequestSanFromSIMCSV
Id: sim
Title: "Santeon Informatiemodel (SIM CSV)"
Description: "Informatieve mapping van FHIR MedicationRequest naar SIM CSV-bronvelden."
Source: MedicationRequestSan
Target: "urn:santeon:sim:csv"

* identifier.system -> "MedicatieAfspraak;MedicatieAfspraakID" "Systeemdeel van de samengestelde identifier."
* identifier.value -> "MedicatieAfspraak;MedicatieAfspraakID" "Waardedeel van de samengestelde identifier; hash."
* subject -> "MedicatieAfspraak;Identificatienummer" "Patient-reference op basis van patiëntidentifier."
* status -> "MedicatieAfspraak;MedicatieAfspraakStatus" "Codes volgens MedicatieAfspraakStatusSysteem; overeenkomst met medicationrequest-status verifiëren."
* medicationCodeableConcept.coding.system -> "MedicatieAfspraak;ProductCodeSysteem"
* medicationCodeableConcept.coding.code -> "MedicatieAfspraak;ProductCode"
* medicationCodeableConcept.coding.display -> "MedicatieAfspraak;ProductNaam"
* dosageInstruction.route.coding.system -> "MedicatieAfspraak;ToedieningswegSystem"
* dosageInstruction.route.coding.code -> "MedicatieAfspraak;ToedieningswegCode"
* dosageInstruction.route.coding.display -> "MedicatieAfspraak;Toedieningsweg"
* dosageInstruction.timing.repeat.boundsPeriod.start -> "MedicatieAfspraak;MedicationRequestDosageInstructionTimingRepeatBoundsPeriodStart"
