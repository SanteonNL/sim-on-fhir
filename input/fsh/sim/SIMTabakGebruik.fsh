Logical: SIMTabakGebruik
Parent: Base
// Parent: GBB - nog te beoordelen
Id: SIMTabakGebruik
Title: "Tabakgebruik (model)"
Description: "Generiek logisch model voor de vastgelegde rookstatus van een patiënt binnen het Santeon Informatiemodel."

// IBD
* MetingID 1..1 Identifier "Uniek ID (key) binnen patiënt per meting"
* PatientIdentificatienummer 1..1 Identifier "Identificatienummer van de patiënt"

* TabakGebruikStatus 0..1 CodeableConcept "Rookstatus van de patiënt"
* TabakGebruikStatus from TabakGebruikStatusCodelijst (extensible)
* DatumVastleggen 0..1 date "Datum waarop de rookstatus is vastgelegd"

// Het SIM-model TabakGebruik bevat geen verdere attributen.

// Terminologie:
// TabakGebruikStatus: TabakGebruikStatusCodelijst uit het SIM
// (versie 2024-04-24). Bindingsterkte extensible; nog te bevestigen
// of alle ziekenhuizen deze lijst leveren.


// Mapping vanuit SIM CSV
Mapping: SIMTabakGebruikFromSIMCSV
Id: SIMTabakGebruikFromSIMCSV
Title: "SIM CSV"
Source: SIMTabakGebruik
Target: "urn:santeon:sim:csv"

// Identificatie
* MetingID -> "TabakGebruik;MetingID"
* PatientIdentificatienummer -> "TabakGebruik;Identificatienummer"

// Rookstatus
* TabakGebruikStatus.coding.system -> "TabakGebruik;TabakGebruikStatusSysteem"
* TabakGebruikStatus.coding.code -> "TabakGebruik;TabakGebruikStatus"
* TabakGebruikStatus.coding.display -> "TabakGebruik;TabakGebruikStatusOmschrijving"

* DatumVastleggen -> "TabakGebruik;TabakGebruikStatusDatumVastleggen"


// Mapping naar FHIR
Mapping: SIMTabakGebruikToFHIR
Id: SIMTabakGebruikToFHIR
Title: "SIMTabakGebruik naar FHIR"
Source: SIMTabakGebruik
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/observation-san-tabakgebruik"

// Identificatie
* MetingID -> "Observation.identifier"
* PatientIdentificatienummer -> "Observation.subject" "Patient-reference op basis van identificatienummer."

// Rookstatus
* TabakGebruikStatus -> "Observation.valueCodeableConcept"
* DatumVastleggen -> "Observation.effectiveDateTime" "Datum uit SIM omzetten naar FHIR dateTime, met behoud van de oorspronkelijke datumprecisie."

// Nog uit te werken:
//
// Observation.status: verplicht in FHIR R4; ontbreekt in de SIM-bron.
//
// Observation.code:
// Geen SIM-bronkolom; ligt vast in ObservationSanTabakGebruik op
// SNOMED CT 365980008, conform Nictiz zib-TobaccoUse.

// GBB: nog te beoordelen of Nictiz GBB een overeenkomstig
// Logical Model voor tabakgebruik biedt.
