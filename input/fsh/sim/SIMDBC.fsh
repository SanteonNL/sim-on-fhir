Logical: SIMDBC
Parent: Base
// Parent: GBB - nog te beoordelen
Id: SIMDBC
Title: "DBC/Zorgtraject (model)"
Description: "Generiek logisch model voor een DBC binnen het Santeon Informatiemodel."

// IBD
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

// Overig van model DBC
// * ZorgtrajectNummer 0..1 Identifier "Identificatienummer van het zorgtraject"
// * Verwijzer 0..1 CodeableConcept "Oorsprong van de verwijzing"

// Terminologie nog te beoordelen:
// Diagnose: coderingssysteem uit SIM bron vaststellen
// SpecialismeDiagnose: Specialisme_DiagnoseCodelijst
// ZorgType: ZorgTypeCode
// ZorgProduct: ZorgProductCode
// UitvoerderSpecialisme: SpecialismeAGBCodelijst
// Verwijzer: bijbehorende ValueSet controleren

// De bestaande Dummy ValueSet-binding voor
// SpecialismeDiagnose wordt pas toegevoegd
// wanneer de definitieve ValueSet is gevalideerd.


// Mapping vanuit SIM CSV
Mapping: SIMDBCFromSIMCSV
Id: SIMDBCFromSIMCSV
Title: "SIM CSV"
Source: SIMDBC
Target: "urn:santeon:sim:csv"

// Identificatie
* PatientIdentificatienummer -> "DBC;Identificatienummer"
* SubtrajectNummer -> "DBC;SubtrajectNr"

// Diagnose
* Diagnose.coding.code -> "DBC;DiagnoseCode"
* Diagnose.coding.display -> "DBC;DiagnoseOmschrijving"

* SpecialismeDiagnose.coding.system -> "DBC;SpecialismeDiagnoseSysteem"
* SpecialismeDiagnose.coding.code -> "DBC;SpecialismeDiagnoseCode"
* SpecialismeDiagnose.coding.display -> "DBC;SpecialismeDiagnoseOmschrijving"

// Zorgtype
* ZorgType.coding.system -> "DBC;ZorgTypeCodeSysteem"
* ZorgType.coding.code -> "DBC;ZorgTypeCode"
* ZorgType.coding.display -> "DBC;ZorgTypeOmschrijving"

// Zorgproduct
* ZorgProduct.coding.system -> "DBC;ZorgProductCodeSysteem"
* ZorgProduct.coding.code -> "DBC;ZorgProductCode"
* ZorgProduct.coding.display -> "DBC;ZorgProductOmschrijving"

// DBC kenmerken
* Gevuld -> "DBC;DBCgevuld"
* Geldig -> "DBC;DBCgeldig"

* OpeningsDatum -> "DBC;DBCOpeningsDatum"
* SluitingsDatum -> "DBC;DBCSluitingsDatum"

// Uitvoerend specialisme
* UitvoerderSpecialisme.system -> "DBC;Uitvoerder_SpecialismeSysteem"
* UitvoerderSpecialisme.code -> "DBC;Uitvoerder_Specialisme"

// Overig van model DBC
// * ZorgtrajectNummer -> "DBC;ZorgtrajectNr"

// * Verwijzer.coding.system -> "DBC;DBCverwijzerCodeSysteem"
// * Verwijzer.coding.code -> "DBC;DBCverwijzerCode"
// * Verwijzer.coding.display -> "DBC;DBCverwijzerCodeOmschrijving"


// Mapping naar FHIR
Mapping: SIMDBCToFHIR
Id: SIMDBCToFHIR
Title: "SIMDBC naar FHIR"
Source: SIMDBC
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/episodeOfCare-san"

// Identificatie
* PatientIdentificatienummer -> "EpisodeOfCare.patient" "Patient-reference op basis van identificatienummer."
* SubtrajectNummer -> "EpisodeOfCare.identifier" "Indien één EpisodeOfCare één DBC-subtraject representeert."

// DBC kenmerken
* ZorgType -> "EpisodeOfCare.type" "Mits het zorgtype de zorgepisode correct classificeert."

// Periode
* OpeningsDatum -> "EpisodeOfCare.period.start" "Alleen indien de DBC-openingsdatum overeenkomt met de start van de zorgepisode."
* SluitingsDatum -> "EpisodeOfCare.period.end" "Alleen indien de DBC-sluitingsdatum overeenkomt met het einde van de zorgepisode."
// Nog uit te werken:

// EpisodeOfCare.status:
// Verplicht in FHIR R4; niet rechtstreeks afleiden uit DBCgeldig.

// Diagnose en SpecialismeDiagnose:
// Kunnen via ConditionSan.code worden gerepresenteerd
// wanneer de codering daadwerkelijk een klinische diagnose beschrijft.
// EpisodeOfCare.diagnosis.condition verwijst naar deze ConditionSan.
//
// TODO:
// - Bepalen of Diagnose of SpecialismeDiagnose de klinische diagnose representeert.
// - Vaststellen hoe de ConditionSan wordt geïdentificeerd of aangemaakt.
// - Vaststellen hoe de specialismecontext behouden blijft.
// - Controleren dat Condition.subject en EpisodeOfCare.patient
//   naar dezelfde patiënt verwijzen.

// ZorgtrajectNummer:
// Relatie tussen zorgtraject en DBC-subtraject uitwerken
// voordat een FHIR-mapping wordt gekozen.

// ZorgProduct, Gevuld, Geldig, UitvoerderSpecialisme en Verwijzer:
// Nog geen directe FHIR-mapping vastgesteld.


// GBB: geen overeenkomstig Logical Model beschikbaar
// in Nictiz GBB 1.0.0-alpha.1.
// GBB-mapping toevoegen zodra een passend generiek
// DBC/EpisodeOfCare-model is gepubliceerd.


