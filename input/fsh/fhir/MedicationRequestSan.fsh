
Profile: MedicationRequestSan
Parent: MedicationRequest
Id: medicationRequest-san
Title: "MedicationRequest (SAN)"
Description: """
Generiek Santeon FHIR R4-profiel voor medicatieafspraken.

Gebaseerd op het Santeon Informatiemodel (SIMMedicatieAfspraak).

De-identificatie wordt uitgevoerd volgens afzonderlijke
transformatiespecificaties en is geen inherente
eigenschap van dit profiel.
"""

// Identifier (SIMMedicatieAfspraak.MedicatieAfspraakID)
* identifier 1..1
* identifier.use 0..0
* identifier.value 1..1
// identifier.system blijft toegestaan, omdat het SIM
// identifier_system|identifier_value ondersteunt.

// MedicatieAfspraakStatus
// MedicationRequest.status is verplicht in FHIR R4 (1..1),
// maar MedicatieAfspraakStatus is nullable in de bron.
// TODO: afleidingsregel vaststellen voor records zonder status.

// MedicationRequest.intent is verplicht in FHIR R4 (1..1).
// De SIM-bron bevat geen intent.
// TODO: herkomst of vaste waarde (bijvoorbeeld order) vaststellen.

// Product
// medication[x] is verplicht in FHIR R4 (1..1),
// maar Product is nullable in de bron.
// Het SIM documenteert het pad via medicationReference naar een
// Medication met de ATC-code; medicationCodeableConcept is het
// eenvoudiger alternatief zonder aparte Medication-resource.
// Beide vormen blijven toegestaan tot de keuze is gemaakt.

// PatientIdentificatienummer
* subject 1..1
* subject only Reference(PatientSan)

// Toedieningsweg en GebruiksperiodeStart
* dosageInstruction 0..*
* dosageInstruction.route 0..1

// VoorschrijverSpecialisme
// requester blijft beschikbaar voor een latere mapping; het SIM
// documenteert requester -> Practitioner.qualification.code.

// Overige FHIR-elementen blijven beschikbaar.
// Toepassingsspecifieke beperkingen kunnen
// in een afgeleid profiel worden opgenomen.


Mapping: MedicationRequestSanFromSIMCSV
Id: sim
Title: "Santeon Informatiemodel (SIM CSV)"
Description: "Informatieve mapping van FHIR MedicationRequest naar SIM CSV-bronvelden."
Source: MedicationRequestSan
Target: "urn:santeon:sim:csv"

// Identificatie
* identifier.system -> "MedicatieAfspraak;MedicatieAfspraakID" "Systeemdeel van de samengestelde identifier."
* identifier.value -> "MedicatieAfspraak;MedicatieAfspraakID" "Waardedeel van de samengestelde identifier; hash."

* subject -> "MedicatieAfspraak;Identificatienummer" "Patient-reference op basis van patiëntidentifier."

// Status
* status -> "MedicatieAfspraak;MedicatieAfspraakStatus" "Codes volgens MedicatieAfspraakStatusSysteem; overeenkomst met medicationrequest-status verifiëren."

// Product (of dezelfde kolommen via medicationReference -> Medication.code)
* medicationCodeableConcept.coding.system -> "MedicatieAfspraak;ProductCodeSysteem"
* medicationCodeableConcept.coding.code -> "MedicatieAfspraak;ProductCode"
* medicationCodeableConcept.coding.display -> "MedicatieAfspraak;ProductNaam"

// Toedieningsweg
* dosageInstruction.route.coding.system -> "MedicatieAfspraak;ToedieningswegSystem"
* dosageInstruction.route.coding.code -> "MedicatieAfspraak;ToedieningswegCode"
* dosageInstruction.route.coding.display -> "MedicatieAfspraak;Toedieningsweg"

// Gebruiksperiode
* dosageInstruction.timing.repeat.boundsPeriod.start -> "MedicatieAfspraak;MedicationRequestDosageInstructionTimingRepeatBoundsPeriodStart"

// Nog uit te werken:
// MedicationRequest.intent: verplicht, geen SIM-bronkolom.
// Doseerduur en DoseerDuurEenheid: SIM geeft geen FHIR-pad.
// timing.repeat.bounds[x] kan niet tegelijk boundsPeriod en
// boundsDuration bevatten; alternatief is
// dispenseRequest.expectedSupplyDuration of een afgeleide
// boundsPeriod.end.
// VoorschrijverSpecialisme: het SIM documenteert het pad via
// requester -> Practitioner.qualification.code.
