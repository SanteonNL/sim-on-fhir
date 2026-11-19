Logical: SIMMedicatieAfspraak
Parent: Base
// Parent: GBB - nog te beoordelen
Id: SIMMedicatieAfspraak
Title: "Medicatieafspraak (model)"
Description: "Generiek logisch model voor een medicatieafspraak binnen het Santeon Informatiemodel."

// IBD
* MedicatieAfspraakID 1..1 Identifier "Uniek ID (key) binnen patiënt per medicatieafspraak"
* PatientIdentificatienummer 1..1 Identifier "Identificatienummer van de patiënt"

* Product 0..1 CodeableConcept "Voorgeschreven geneesmiddel"
* Product ^definition = "Voorgeschreven geneesmiddel, gecodeerd met ATC. De IBD-indicatoren selecteren op ATC-code."

* Toedieningsweg 0..1 CodeableConcept "Route waarlangs de medicatie wordt toegediend"

* Doseerduur 0..1 Duration "Duur waarvoor de medicatie is voorgeschreven"
* GebruiksperiodeStart 0..1 dateTime "Datum en eventueel tijd waarop de gebruiksinstructie van deze medicatieafspraak ingaat"

* MedicatieAfspraakStatus 0..1 Coding "Status van de medicatieafspraak"

* VoorschrijverSpecialisme 0..1 CodeableConcept "Medisch specialisme van de voorschrijver"

// Overig van model MedicatieAfspraak
// * ProductLabel 0..1 string "Tijdelijk attribuut voor geneesmiddelomschrijving"
// * MedicationID 0..1 Identifier "Verwijzing naar het geneesmiddel (Z-indexnummer)"
// * MedicatieAfspraakDatumTijd 0..1 dateTime "Datum en eventueel tijd waarop het voorschrift is uitgeschreven"
// * GebruiksperiodeEind 0..1 dateTime "Datum en eventueel tijd waarop de gebruiksperiode eindigt"
// * KeerDosis 0..1 Quantity "Hoeveelheid per toediening"
// * DoseerFrequentie 0..1 Quantity "Frequentie van toediening"
// * FrequentieBereik 0..1 code "Bereik van de doseerfrequentie"

// Terminologie nog te beoordelen:
// Product: ATC; ProductCodeSysteem controleren
// Toedieningsweg: bijbehorende ValueSet controleren
// MedicatieAfspraakStatus: in de IBD-indicatoren active, completed, stopped, cancelled
//   (komt overeen met FHIR medicationrequest-status)
// VoorschrijverSpecialisme: SpecialismeAGBCodelijst


// Mapping vanuit SIM CSV
Mapping: SIMMedicatieAfspraakFromSIMCSV
Id: SIMMedicatieAfspraakFromSIMCSV
Title: "SIM CSV"
Source: SIMMedicatieAfspraak
Target: "urn:santeon:sim:csv"

// Identificatie
* MedicatieAfspraakID -> "MedicatieAfspraak;MedicatieAfspraakID"
* PatientIdentificatienummer -> "MedicatieAfspraak;Identificatienummer"

// Product
* Product.coding.system -> "MedicatieAfspraak;ProductCodeSysteem"
* Product.coding.code -> "MedicatieAfspraak;ProductCode"
* Product.coding.display -> "MedicatieAfspraak;ProductNaam"

// Toedieningsweg
// De IBD-indicatoren vragen de kolom Toedieningsweg (omschrijving) uit.
// SIM kent daarnaast ToedieningswegDisplay met dezelfde betekenis.
* Toedieningsweg.coding.system -> "MedicatieAfspraak;ToedieningswegSystem"
* Toedieningsweg.coding.code -> "MedicatieAfspraak;ToedieningswegCode"
* Toedieningsweg.coding.display -> "MedicatieAfspraak;Toedieningsweg"

// Doseerduur en gebruiksperiode
* Doseerduur.value -> "MedicatieAfspraak;Doseerduur"
* Doseerduur.code -> "MedicatieAfspraak;DoseerDuurEenheid"
* GebruiksperiodeStart -> "MedicatieAfspraak;MedicationRequestDosageInstructionTimingRepeatBoundsPeriodStart"

// Status
* MedicatieAfspraakStatus.system -> "MedicatieAfspraak;MedicatieAfspraakStatusSysteem"
* MedicatieAfspraakStatus.code -> "MedicatieAfspraak;MedicatieAfspraakStatus"

// Voorschrijver
* VoorschrijverSpecialisme.coding.system -> "MedicatieAfspraak;Voorschrijver_SpecialismeSysteem"
* VoorschrijverSpecialisme.coding.code -> "MedicatieAfspraak;Voorschrijver_Specialisme"

// Overig van model MedicatieAfspraak
// * ProductLabel -> "MedicatieAfspraak;ProductLabel"
// * MedicationID -> "MedicatieAfspraak;MedicationID"
// * MedicatieAfspraakDatumTijd -> "MedicatieAfspraak;MedicatieAfspraakDatumTijd"
// * GebruiksperiodeEind -> "MedicatieAfspraak;MedicationRequestDosageInstructionTimingRepeatBoundsPeriodEnd"
// * KeerDosis.value -> "MedicatieAfspraak;KeerDosis"
// * KeerDosis.code -> "MedicatieAfspraak;KeerDosisEenheid"
// * DoseerFrequentie.value -> "MedicatieAfspraak;DoseerFrequentie"
// * DoseerFrequentie.code -> "MedicatieAfspraak;DoseerFrequentieEenheid"
// * FrequentieBereik -> "MedicatieAfspraak;FrequentieBereik"


// Mapping naar FHIR

Mapping: SIMMedicatieAfspraakToFHIR
Id: SIMMedicatieAfspraakToFHIR
Title: "SIMMedicatieAfspraak naar FHIR"
Source: SIMMedicatieAfspraak
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/medicationRequest-san"

// Identificatie
* MedicatieAfspraakID -> "MedicationRequest.identifier"
* PatientIdentificatienummer -> "MedicationRequest.subject" "Patient-reference op basis van identificatienummer."

// Product
* Product -> "MedicationRequest.medicationCodeableConcept" "Het SIM documenteert medicationReference naar een Medication met deze code; keuze nog te maken."

// Dosering
* Toedieningsweg -> "MedicationRequest.dosageInstruction.route"
* GebruiksperiodeStart -> "MedicationRequest.dosageInstruction.timing.repeat.boundsPeriod.start"

// Status
* MedicatieAfspraakStatus -> "MedicationRequest.status"

// Nog uit te werken:
//
// MedicationRequest.intent: verplicht in FHIR R4; ontbreekt in de SIM-bron.
//
// Doseerduur:
// SIM geeft geen FHIR-pad. timing.repeat.bounds[x] kan niet tegelijk
// boundsPeriod (GebruiksperiodeStart) en boundsDuration bevatten.
// Alternatief: dispenseRequest.expectedSupplyDuration, of de einddatum
// afleiden en als boundsPeriod.end opnemen.
//
// VoorschrijverSpecialisme:
// Het SIM documenteert MedicationRequest.requester ->
// Practitioner.qualification.code.

// GBB: nog te beoordelen of Nictiz GBB een overeenkomstig
// Logical Model voor medicatieafspraak biedt.
