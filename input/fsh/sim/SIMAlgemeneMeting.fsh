Logical: SIMAlgemeneMeting
Parent: Base
Id: SIMAlgemeneMeting
Title: "Algemene meting (model)"
Description: "Generiek logisch model voor een algemene meting binnen het Santeon Informatiemodel."

// IBD-scope
* MetingID 1..1 Identifier "Uniek ID (key) binnen patiënt per meting"
* PatientIdentificatienummer 1..1 Identifier "Identificatienummer van de patiënt"
* MetingNaam 1..1 CodeableConcept "Type of naam van de meting"
* MetingDatumTijd 0..1 dateTime "Datum en eventueel tijd die het dichtst ligt bij het daadwerkelijke meetmoment of de observatie"
* UitslagWaarde 0..1 Quantity "Numerieke uitslag van de meting, met eenheid (UCUM) en eventueel een vergelijkingsoperator"
* UitslagBereik 0..1 Range "Uitslag als bereik: een ondergrens en/of een bovengrens, beide inclusief"
* UitslagCode 0..1 CodeableConcept "Gecodeerde uitslag van de meting"
* UitslagCode ^definition = "Welke ValueSet van toepassing is op UitslagCode wordt bepaald door MetingNaam. De bijbehorende terminologierelaties worden afzonderlijk beheerd."
* UitslagDatumTijd 0..1 dateTime "Datum en eventueel tijd als antwoord op een vraag, bijvoorbeeld: sinds wanneer rookt u? Een vage datumtijd (alleen jaar of datum) is niet wenselijk maar wel toegestaan."
* Meetmethode 0..1 CodeableConcept "Methode waarmee de meting is uitgevoerd"
// Een meting heeft maximaal één uitslagtype (UitslagWaarde, UitslagBereik,
// UitslagCode of UitslagDatumTijd).

// Terminologie
* MetingNaam from Santeon_MetingNaamCodelijst (required)
// UitslagCode: ValueSet afhankelijk van MetingNaam (zie de definitie van UitslagCode).
// Meetmethode: codeersysteem en ValueSet nog te beoordelen.



// ============================================
// MAPPING 1: Herkomst - SIM CSV
// ============================================

Mapping: SIMAlgemeneMetingFromSIMCSV
Id: SIMAlgemeneMetingFromSIMCSV
Title: "SIM CSV"
Description: "Herleidbaarheid van SIMAlgemeneMeting-elementen naar de oorspronkelijke SIM CSV-attributen."
Source: SIMAlgemeneMeting
Target: "urn:santeon:sim:csv"

// IBD-scope
* MetingID -> "AlgemeneMeting;MetingID"
* PatientIdentificatienummer -> "AlgemeneMeting;Identificatienummer"
* MetingNaam.coding.system -> "AlgemeneMeting;MetingNaamCodeSysteem"
* MetingNaam.coding.code -> "AlgemeneMeting;MetingNaamCode"
* MetingNaam.coding.display -> "AlgemeneMeting;MetingNaamOmschrijving"
* MetingDatumTijd -> "AlgemeneMeting;MetingDatumTijd"
* UitslagWaarde.value -> "AlgemeneMeting;UitslagWaarde"
* UitslagWaarde.system -> "AlgemeneMeting;UitslagWaardeEenheidSysteem"
* UitslagWaarde.code -> "AlgemeneMeting;UitslagWaardeEenheid"
* UitslagWaarde.comparator -> "AlgemeneMeting;UitslagWaardeOperator"
* UitslagBereik.low.value -> "AlgemeneMeting;ObservationValueRangeLow"
* UitslagBereik.low.unit -> "AlgemeneMeting;ObservationValueRangeLowUnit"
* UitslagBereik.low.system -> "AlgemeneMeting;ObservationValueRangeLowSystem"
* UitslagBereik.low.code -> "AlgemeneMeting;ObservationValueRangeLowCode"
* UitslagBereik.high.value -> "AlgemeneMeting;ObservationValueRangeHigh"
* UitslagBereik.high.unit -> "AlgemeneMeting;ObservationValueRangeHighUnit"
* UitslagBereik.high.system -> "AlgemeneMeting;ObservationValueRangeHighSystem"
* UitslagBereik.high.code -> "AlgemeneMeting;ObservationValueRangeHighCode"
* UitslagCode.coding.system -> "AlgemeneMeting;UitslagCodeSysteem"
* UitslagCode.coding.code -> "AlgemeneMeting;UitslagCode"
* UitslagCode.coding.display -> "AlgemeneMeting;UitslagCodeOmschrijving"
* UitslagDatumTijd -> "AlgemeneMeting;UitslagDatumTijd"
* Meetmethode.coding.system -> "AlgemeneMeting;MeetmethodeCodeSysteem"
* Meetmethode.coding.code -> "AlgemeneMeting;MeetmethodeCode"
* Meetmethode.coding.display -> "AlgemeneMeting;MeetmethodeOmschrijving"


// ============================================
// MAPPING 2: Semantische delta-analyse - SIMAlgemeneMeting naar Nictiz GBB-afsprakenmodel
// ============================================

// Geen mapping: Nictiz GBB (referentieversie 1.0.0-alpha.2) bevat geen
// overeenkomstig afsprakenmodel voor een generieke meting (Observation).
// Ook in 1.0.0-alpha.1 was dit niet beschikbaar.


// ============================================
// MAPPING 3: Technische vertaling, harmonisatie en conversie - SIMAlgemeneMeting naar ObservationSan
// De omgekeerde mapping (ObservationSan naar SIMAlgemeneMeting) wordt automatisch gegenereerd.
// ============================================

Mapping: SIMAlgemeneMetingToFHIR
Id: SIMAlgemeneMetingToFHIR
Title: "FHIR-mapping naar ObservationSan"
Description: "Mapping van SIMAlgemeneMeting-elementen naar het Santeon FHIR-profiel ObservationSan, inclusief harmonisatie met FHIR-standaarden en toelichting op benodigde conversies."
Source: SIMAlgemeneMeting
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/observation-san"

// IBD-scope
* MetingID -> "Observation.identifier" "Direct: samengestelde identifier (system|value); het waardedeel wordt gehasht volgens de ETL-specificatie."
* PatientIdentificatienummer -> "Observation.subject" "Structuurconversie: Patient-reference op basis van identificatienummer."
* MetingNaam -> "Observation.code" "Direct: SIM CodeableConcept wordt overgenomen als FHIR CodeableConcept."
* MetingDatumTijd -> "Observation.effectiveDateTime" "Direct: SIM dateTime wordt overgenomen als FHIR dateTime."
* UitslagWaarde -> "Observation.valueQuantity" "Direct: SIM Quantity wordt overgenomen als FHIR Quantity."
* UitslagBereik -> "Observation.valueRange" "Direct: SIM Range wordt overgenomen als FHIR Range."
* UitslagCode -> "Observation.valueCodeableConcept" "Direct: SIM CodeableConcept wordt overgenomen als FHIR CodeableConcept."
* UitslagDatumTijd -> "Observation.valueDateTime" "Direct: SIM dateTime wordt overgenomen als FHIR dateTime."
* Meetmethode -> "Observation.method" "Direct: SIM CodeableConcept wordt overgenomen als FHIR CodeableConcept."


// ============================================
// Status en openstaande controles
// ============================================

// Status: In progress
// TODO: Observation.status is verplicht in FHIR R4 maar ontbreekt in de SIM-bron; herkomst of gevalideerde afleidingsregel vaststellen.
// TODO: Selectie van maximaal één Observation.value[x] per meting uitwerken.
// TODO: Toegestane uitslagtypen en ValueSets per MetingNaam uitwerken met ObservationDefinition en de SIM-terminologierelaties.
// TODO: Meetmethode: codeersysteem en ValueSet controleren.
// TODO: GBB-delta-analyse uitvoeren zodra Nictiz een passend generiek Observation/Measurement-afsprakenmodel publiceert.
