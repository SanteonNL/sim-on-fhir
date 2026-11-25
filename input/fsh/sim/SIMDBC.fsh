Logical: SIMDBC
Parent: Base
Id: SIMDBC
Title: "DBC/Zorgtraject (model)"
Description: "Generiek logisch model voor een DBC binnen het Santeon Informatiemodel."

// IBD-scope
* PatientIdentificatienummer 1..1 Identifier "Identificatienummer van de patiënt"
* SubtrajectNummer 0..1 Identifier "Uniek identificatienummer van het DBC-subtraject"
* Diagnose 0..1 CodeableConcept "Diagnose behorend bij de DBC"
* SpecialismeDiagnose 0..1 CodeableConcept "Specialisme en diagnose behorend bij de DBC"
* ZorgType 0..1 CodeableConcept "Zorgtype van de DBC"
* ZorgProduct 0..1 CodeableConcept "Zorgproduct van de DBC"
* Gevuld 0..1 boolean "Geeft aan of de DBC verrichtingen bevat"
* Geldig 0..1 boolean "Geeft aan of de DBC administratief geldig is"
* OpeningsDatum 0..1 date "Openingsdatum van de DBC"
* SluitingsDatum 0..1 date "Sluitingsdatum van de DBC"
* UitvoerderSpecialisme 0..1 Coding "Specialisme van de uitvoerder"

// Buiten IBD-scope - nog niet meegenomen
// * ZorgtrajectNummer 0..1 Identifier "Identificatienummer van het zorgtraject"
// * Verwijzer 0..1 CodeableConcept "Oorsprong van de verwijzing"

// Terminologie
// Nog geen actieve bindingen. Te beoordelen:
// Diagnose: coderingssysteem uit SIM-bron vaststellen.
// SpecialismeDiagnose: Specialisme_DiagnoseCodelijst. De bestaande Dummy ValueSet-binding wordt pas toegevoegd wanneer de definitieve ValueSet is gevalideerd.
// ZorgType: ZorgTypeCode.
// ZorgProduct: ZorgProductCode.
// UitvoerderSpecialisme: SpecialismeAGBCodelijst.
// Verwijzer: bijbehorende ValueSet controleren.



// ============================================
// MAPPING 1: Herkomst - SIM CSV
// ============================================

Mapping: SIMDBCFromSIMCSV
Id: SIMDBCFromSIMCSV
Title: "SIM CSV"
Description: "Herleidbaarheid van SIMDBC-elementen naar de oorspronkelijke SIM CSV-attributen."
Source: SIMDBC
Target: "urn:santeon:sim:csv"

// IBD-scope
* PatientIdentificatienummer -> "DBC;Identificatienummer"
* SubtrajectNummer -> "DBC;SubtrajectNr"
* Diagnose.coding.code -> "DBC;DiagnoseCode"
* Diagnose.coding.display -> "DBC;DiagnoseOmschrijving"
* SpecialismeDiagnose.coding.system -> "DBC;SpecialismeDiagnoseSysteem"
* SpecialismeDiagnose.coding.code -> "DBC;SpecialismeDiagnoseCode"
* SpecialismeDiagnose.coding.display -> "DBC;SpecialismeDiagnoseOmschrijving"
* ZorgType.coding.system -> "DBC;ZorgTypeCodeSysteem"
* ZorgType.coding.code -> "DBC;ZorgTypeCode"
* ZorgType.coding.display -> "DBC;ZorgTypeOmschrijving"
* ZorgProduct.coding.system -> "DBC;ZorgProductCodeSysteem"
* ZorgProduct.coding.code -> "DBC;ZorgProductCode"
* ZorgProduct.coding.display -> "DBC;ZorgProductOmschrijving"
* Gevuld -> "DBC;DBCgevuld"
* Geldig -> "DBC;DBCgeldig"
* OpeningsDatum -> "DBC;DBCOpeningsDatum"
* SluitingsDatum -> "DBC;DBCSluitingsDatum"
* UitvoerderSpecialisme.system -> "DBC;Uitvoerder_SpecialismeSysteem"
* UitvoerderSpecialisme.code -> "DBC;Uitvoerder_Specialisme"

// Buiten IBD-scope - nog niet meegenomen
// * ZorgtrajectNummer -> "DBC;ZorgtrajectNr"
// * Verwijzer.coding.system -> "DBC;DBCverwijzerCodeSysteem"
// * Verwijzer.coding.code -> "DBC;DBCverwijzerCode"
// * Verwijzer.coding.display -> "DBC;DBCverwijzerCodeOmschrijving"


// ============================================
// MAPPING 2: Semantische delta-analyse - SIMDBC naar Nictiz GBB-afsprakenmodel
// ============================================

// Geen mapping: Nictiz GBB (referentieversie 1.0.0-alpha.2) bevat geen
// overeenkomstig afsprakenmodel voor een DBC of zorgepisode (EpisodeOfCare).
// Ook in 1.0.0-alpha.1 was dit niet beschikbaar.
// GBB bevat wel LogicalModelCondition; een vergelijking van Diagnose met dat
// model is nog niet uitgevoerd (zie Status en openstaande controles).


// ============================================
// MAPPING 3: Technische vertaling, harmonisatie en conversie - SIMDBC naar EpisodeOfCareSan
// De omgekeerde mapping (EpisodeOfCareSan naar SIMDBC) wordt automatisch gegenereerd.
// ============================================

Mapping: SIMDBCToFHIR
Id: SIMDBCToFHIR
Title: "FHIR-mapping naar EpisodeOfCareSan"
Description: "Mapping van SIMDBC-elementen naar het Santeon FHIR-profiel EpisodeOfCareSan, inclusief harmonisatie met FHIR-standaarden en toelichting op benodigde conversies."
Source: SIMDBC
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/episodeOfCare-san"

// IBD-scope
* PatientIdentificatienummer -> "EpisodeOfCare.patient" "Structuurconversie: Patient-reference op basis van identificatienummer."
* SubtrajectNummer -> "EpisodeOfCare.identifier" "Direct: indien één EpisodeOfCare één DBC-subtraject representeert; samengestelde identifier (system|value), waardedeel gehasht volgens de ETL-specificatie."
* ZorgType -> "EpisodeOfCare.type" "Direct: mits het zorgtype de zorgepisode correct classificeert."
* OpeningsDatum -> "EpisodeOfCare.period.start" "Datatypeconversie: SIM date naar FHIR dateTime met behoud van datumprecisie (geen tijdstip toevoegen); alleen indien de DBC-openingsdatum overeenkomt met de start van de zorgepisode."
* SluitingsDatum -> "EpisodeOfCare.period.end" "Datatypeconversie: SIM date naar FHIR dateTime met behoud van datumprecisie (geen tijdstip toevoegen); alleen indien de DBC-sluitingsdatum overeenkomt met het einde van de zorgepisode."


// ============================================
// Status en openstaande controles
// ============================================

// Status: In progress
// TODO: EpisodeOfCare.status is verplicht in FHIR R4; niet rechtstreeks afleiden uit DBCgeldig.
// TODO: Diagnose en SpecialismeDiagnose kunnen via ConditionSan.code worden gerepresenteerd wanneer de codering daadwerkelijk een klinische diagnose beschrijft;
//   EpisodeOfCare.diagnosis.condition verwijst dan naar deze ConditionSan. Nog bepalen:
//   - of Diagnose of SpecialismeDiagnose de klinische diagnose representeert;
//   - hoe de ConditionSan wordt geïdentificeerd of aangemaakt;
//   - hoe de specialismecontext behouden blijft;
//   - dat Condition.subject en EpisodeOfCare.patient naar dezelfde patiënt verwijzen.
// TODO: ZorgtrajectNummer (buiten IBD-scope): relatie tussen zorgtraject en DBC-subtraject uitwerken voordat een FHIR-mapping wordt gekozen.
// TODO: ZorgProduct, Gevuld, Geldig, UitvoerderSpecialisme en Verwijzer: nog geen directe FHIR-mapping vastgesteld.
// TODO: Terminologie voor Diagnose, SpecialismeDiagnose, ZorgType, ZorgProduct, UitvoerderSpecialisme en Verwijzer beoordelen (zie Terminologie).
// TODO: Beoordelen of Diagnose semantisch kan worden vergeleken met Nictiz GBB LogicalModelCondition (1.0.0-alpha.2).
