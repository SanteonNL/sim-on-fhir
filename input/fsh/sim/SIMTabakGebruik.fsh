Logical: SIMTabakGebruik
Parent: Base
Id: SIMTabakGebruik
Title: "Tabakgebruik (model)"
Description: "Generiek logisch model voor de vastgelegde rookstatus van een patiënt binnen het Santeon Informatiemodel."

// IBD-scope
* MetingID 1..1 Identifier "Uniek ID (key) binnen patiënt per meting"
* PatientIdentificatienummer 1..1 Identifier "Identificatienummer van de patiënt"
* TabakGebruikStatus 0..1 CodeableConcept "Rookstatus van de patiënt"
* DatumVastleggen 0..1 date "Datum waarop de rookstatus is vastgelegd"
// Het SIM-model TabakGebruik bevat geen verdere attributen.

// Terminologie
* TabakGebruikStatus from TabakGebruikStatusCodelijst (extensible)
// TabakGebruikStatusCodelijst uit het SIM (versie 2024-04-24).



// ============================================
// MAPPING 1: Herkomst - SIM CSV
// ============================================

Mapping: SIMTabakGebruikFromSIMCSV
Id: SIMTabakGebruikFromSIMCSV
Title: "SIM CSV"
Description: "Herleidbaarheid van SIMTabakGebruik-elementen naar de oorspronkelijke SIM CSV-attributen."
Source: SIMTabakGebruik
Target: "urn:santeon:sim:csv"

// IBD-scope
* MetingID -> "TabakGebruik;MetingID"
* PatientIdentificatienummer -> "TabakGebruik;Identificatienummer"
* TabakGebruikStatus.coding.system -> "TabakGebruik;TabakGebruikStatusSysteem"
* TabakGebruikStatus.coding.code -> "TabakGebruik;TabakGebruikStatus"
* TabakGebruikStatus.coding.display -> "TabakGebruik;TabakGebruikStatusOmschrijving"
* DatumVastleggen -> "TabakGebruik;TabakGebruikStatusDatumVastleggen"


// ============================================
// MAPPING 2: Semantische delta-analyse - SIMTabakGebruik naar Nictiz GBB-afsprakenmodel
// ============================================

// Geen mapping: Nictiz GBB (referentieversie 1.0.0-alpha.2) bevat geen
// overeenkomstig afsprakenmodel voor tabakgebruik.


// ============================================
// MAPPING 3: Technische vertaling, harmonisatie en conversie - SIMTabakGebruik naar ObservationSanTabakGebruik
// De omgekeerde mapping (ObservationSanTabakGebruik naar SIMTabakGebruik) wordt automatisch gegenereerd.
// ============================================

Mapping: SIMTabakGebruikToFHIR
Id: SIMTabakGebruikToFHIR
Title: "FHIR-mapping naar ObservationSanTabakGebruik"
Description: "Mapping van SIMTabakGebruik-elementen naar het Santeon FHIR-profiel ObservationSanTabakGebruik, inclusief harmonisatie met FHIR-standaarden en toelichting op benodigde conversies."
Source: SIMTabakGebruik
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/observation-san-tabakgebruik"

// IBD-scope
* MetingID -> "Observation.identifier" "Direct: samengestelde identifier (system|value); het waardedeel wordt gehasht volgens de ETL-specificatie."
* PatientIdentificatienummer -> "Observation.subject" "Structuurconversie: Patient-reference op basis van identificatienummer."
* TabakGebruikStatus -> "Observation.valueCodeableConcept" "Direct: SIM CodeableConcept wordt overgenomen als FHIR CodeableConcept, gebonden aan TabakGebruikStatusCodelijst."
* DatumVastleggen -> "Observation.effectiveDateTime" "Datatypeconversie: SIM date naar FHIR dateTime, met behoud van de oorspronkelijke datumprecisie."
// Observation.code heeft geen SIM-bronkolom; de code ligt vast in
// ObservationSanTabakGebruik (SNOMED CT 365980008, conform Nictiz zib-TobaccoUse).


// ============================================
// Status en openstaande controles
// ============================================

// Status: In progress
// TODO: Observation.status is verplicht in FHIR R4 en ontbreekt in de SIM-bron.
// TODO: Bindingsterkte extensible van TabakGebruikStatusCodelijst bevestigen; nagaan of alle ziekenhuizen deze lijst leveren.
