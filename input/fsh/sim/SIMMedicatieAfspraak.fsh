Logical: SIMMedicatieAfspraak
Parent: Base
Id: SIMMedicatieAfspraak
Title: "Medicatieafspraak (model)"
Description: "Generiek logisch model voor een medicatieafspraak binnen het Santeon Informatiemodel."

// IBD-scope
* MedicatieAfspraakID 1..1 Identifier "Uniek ID (key) binnen patiënt per medicatieafspraak"
* PatientIdentificatienummer 1..1 Identifier "Identificatienummer van de patiënt"
* Product 0..1 CodeableConcept "Voorgeschreven geneesmiddel"
* Product ^definition = "Voorgeschreven geneesmiddel, gecodeerd met ATC. De IBD-indicatoren selecteren op ATC-code."
* Toedieningsweg 0..1 CodeableConcept "Route waarlangs de medicatie wordt toegediend"
* Doseerduur 0..1 Duration "Duur waarvoor de medicatie is voorgeschreven"
* GebruiksperiodeStart 0..1 dateTime "Datum en eventueel tijd waarop de gebruiksinstructie van deze medicatieafspraak ingaat"
* MedicatieAfspraakStatus 0..1 Coding "Status van de medicatieafspraak"
* VoorschrijverSpecialisme 0..1 CodeableConcept "Medisch specialisme van de voorschrijver"

// Buiten IBD-scope - nog niet meegenomen
// * ProductLabel 0..1 string "Tijdelijk attribuut voor geneesmiddelomschrijving"
// * MedicationID 0..1 Identifier "Verwijzing naar het geneesmiddel (Z-indexnummer)"
// * MedicatieAfspraakDatumTijd 0..1 dateTime "Datum en eventueel tijd waarop het voorschrift is uitgeschreven"
// * GebruiksperiodeEind 0..1 dateTime "Datum en eventueel tijd waarop de gebruiksperiode eindigt"
// * KeerDosis 0..1 Quantity "Hoeveelheid per toediening"
// * DoseerFrequentie 0..1 Quantity "Frequentie van toediening"
// * FrequentieBereik 0..1 code "Bereik van de doseerfrequentie"

// Terminologie
// Nog geen actieve bindingen. Te beoordelen:
// Product: ATC; ProductCodeSysteem controleren.
// Toedieningsweg: bijbehorende ValueSet controleren.
// MedicatieAfspraakStatus: in de IBD-indicatoren active, completed, stopped, cancelled (komt overeen met FHIR medicationrequest-status).
// VoorschrijverSpecialisme: SpecialismeAGBCodelijst.



// ============================================
// MAPPING 1: Herkomst - SIM CSV
// ============================================

Mapping: SIMMedicatieAfspraakFromSIMCSV
Id: SIMMedicatieAfspraakFromSIMCSV
Title: "SIM CSV"
Description: "Herleidbaarheid van SIMMedicatieAfspraak-elementen naar de oorspronkelijke SIM CSV-attributen."
Source: SIMMedicatieAfspraak
Target: "urn:santeon:sim:csv"

// IBD-scope
* MedicatieAfspraakID -> "MedicatieAfspraak;MedicatieAfspraakID"
* PatientIdentificatienummer -> "MedicatieAfspraak;Identificatienummer"
* Product.coding.system -> "MedicatieAfspraak;ProductCodeSysteem"
* Product.coding.code -> "MedicatieAfspraak;ProductCode"
* Product.coding.display -> "MedicatieAfspraak;ProductNaam"
// De IBD-indicatoren vragen de kolom Toedieningsweg (omschrijving) uit.
// SIM kent daarnaast ToedieningswegDisplay met dezelfde betekenis.
* Toedieningsweg.coding.system -> "MedicatieAfspraak;ToedieningswegSystem"
* Toedieningsweg.coding.code -> "MedicatieAfspraak;ToedieningswegCode"
* Toedieningsweg.coding.display -> "MedicatieAfspraak;Toedieningsweg"
* Doseerduur.value -> "MedicatieAfspraak;Doseerduur"
* Doseerduur.code -> "MedicatieAfspraak;DoseerDuurEenheid"
* GebruiksperiodeStart -> "MedicatieAfspraak;MedicationRequestDosageInstructionTimingRepeatBoundsPeriodStart"
* MedicatieAfspraakStatus.system -> "MedicatieAfspraak;MedicatieAfspraakStatusSysteem"
* MedicatieAfspraakStatus.code -> "MedicatieAfspraak;MedicatieAfspraakStatus"
* VoorschrijverSpecialisme.coding.system -> "MedicatieAfspraak;Voorschrijver_SpecialismeSysteem"
* VoorschrijverSpecialisme.coding.code -> "MedicatieAfspraak;Voorschrijver_Specialisme"

// Buiten IBD-scope - nog niet meegenomen
// * ProductLabel -> "MedicatieAfspraak;ProductLabel"
// * MedicationID -> "MedicatieAfspraak;MedicationID"
// * MedicatieAfspraakDatumTijd -> "MedicatieAfspraak;MedicatieAfspraakDatumTijd"
// * GebruiksperiodeEind -> "MedicatieAfspraak;MedicationRequestDosageInstructionTimingRepeatBoundsPeriodEnd"
// * KeerDosis.value -> "MedicatieAfspraak;KeerDosis"
// * KeerDosis.code -> "MedicatieAfspraak;KeerDosisEenheid"
// * DoseerFrequentie.value -> "MedicatieAfspraak;DoseerFrequentie"
// * DoseerFrequentie.code -> "MedicatieAfspraak;DoseerFrequentieEenheid"
// * FrequentieBereik -> "MedicatieAfspraak;FrequentieBereik"


// ============================================
// MAPPING 2: Semantische delta-analyse - SIMMedicatieAfspraak naar Nictiz GBB-afsprakenmodel
// ============================================

// Geen mapping: Nictiz GBB (referentieversie 1.0.0-alpha.2) bevat geen
// overeenkomstig afsprakenmodel voor een medicatieafspraak.


// ============================================
// MAPPING 3: Technische vertaling, harmonisatie en conversie - SIMMedicatieAfspraak naar MedicationRequestSan
// De omgekeerde mapping (MedicationRequestSan naar SIMMedicatieAfspraak) wordt automatisch gegenereerd.
// ============================================

Mapping: SIMMedicatieAfspraakToFHIR
Id: SIMMedicatieAfspraakToFHIR
Title: "FHIR-mapping naar MedicationRequestSan"
Description: "Mapping van SIMMedicatieAfspraak-elementen naar het Santeon FHIR-profiel MedicationRequestSan, inclusief harmonisatie met FHIR-standaarden en toelichting op benodigde conversies."
Source: SIMMedicatieAfspraak
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/medicationRequest-san"

// IBD-scope
* MedicatieAfspraakID -> "MedicationRequest.identifier" "Direct: samengestelde identifier (system|value); het waardedeel wordt gehasht volgens de ETL-specificatie."
* PatientIdentificatienummer -> "MedicationRequest.subject" "Structuurconversie: Patient-reference op basis van identificatienummer."
* Product -> "MedicationRequest.medicationCodeableConcept" "Direct: SIM CodeableConcept wordt overgenomen als FHIR CodeableConcept. Het SIM documenteert medicationReference naar een Medication met deze code; keuze nog te maken."
* Toedieningsweg -> "MedicationRequest.dosageInstruction.route" "Direct: SIM CodeableConcept wordt overgenomen als FHIR CodeableConcept."
* GebruiksperiodeStart -> "MedicationRequest.dosageInstruction.timing.repeat.boundsPeriod.start" "Direct: SIM dateTime wordt overgenomen als FHIR dateTime."
* MedicatieAfspraakStatus -> "MedicationRequest.status" "Datatypeconversie: SIM Coding naar FHIR code; codes volgens MedicatieAfspraakStatusSysteem, overeenkomst met FHIR medicationrequest-status verifiëren."


// ============================================
// Status en openstaande controles
// ============================================

// Status: In progress
// TODO: MedicationRequest.status is verplicht in FHIR R4, maar MedicatieAfspraakStatus is nullable in de bron; afleidingsregel vaststellen voor records zonder status.
// TODO: MedicationRequest.intent is verplicht in FHIR R4 en ontbreekt in de SIM-bron.
// TODO: Doseerduur: het SIM geeft geen FHIR-pad. timing.repeat.bounds[x] kan niet tegelijk boundsPeriod (GebruiksperiodeStart) en boundsDuration bevatten. Alternatief: dispenseRequest.expectedSupplyDuration, of de einddatum afleiden en als boundsPeriod.end opnemen.
// TODO: VoorschrijverSpecialisme: het SIM documenteert MedicationRequest.requester -> Practitioner.qualification.code.
// TODO: Terminologie voor Product, Toedieningsweg, MedicatieAfspraakStatus en VoorschrijverSpecialisme beoordelen (zie Terminologie).
