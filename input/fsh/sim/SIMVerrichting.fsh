Logical: SIMVerrichting
Parent: Base
Id: SIMVerrichting
Title: "Verrichting (model)"
Description: "Generiek logisch model voor een verrichting binnen het Santeon Informatiemodel."

// IBD-scope
* VerrichtingID 1..1 Identifier "Uniek ID (key) binnen patiënt per verrichting"
* ContactID 0..1 Identifier "Uniek ID (key) binnen patiënt per contact"
* OperatieID 0..1 Identifier "Uniek ID (key) binnen patiënt per OK"
* SubtrajectNummer 0..1 Identifier "Identificatienummer van het DBC-subtraject"
* PatientIdentificatienummer 1..1 Identifier "Identificatienummer van de patiënt"
* VerrichtingType 0..1 CodeableConcept "Type verrichting"
* VerrichtingType ^definition = "Type verrichting, gecodeerd met NZa, SNOMED CT en/of DHD. Meerdere coderingen worden alleen gecombineerd wanneer zij hetzelfde verrichtingsconcept vertegenwoordigen."
* VerrichtingAantal 0..1 integer "Aantal verrichtingen"
* VerrichtingStartDatum 0..1 dateTime "Datum en eventueel tijd waarop de verrichting is gestart"
* AanvragerSpecialisme 0..1 CodeableConcept "Medisch specialisme dat de verrichting heeft aangevraagd"
* UitvoerderSpecialisme 0..1 CodeableConcept "Medisch specialisme dat de verrichting heeft uitgevoerd"
* VerrichtingCategorie 0..1 CodeableConcept "Duiding tot welke zorgprofielklasse een verrichting behoort"

// Buiten IBD-scope - nog niet meegenomen
// * VerrichtingEindDatum 0..1 dateTime "Datum en eventueel tijd waarop de verrichting is beëindigd"

// Terminologie
// Nog geen actieve bindingen. Te beoordelen:
// VerrichtingType: NZa, SNOMED CT, DHD.
// AanvragerSpecialisme: SpecialismeAGBCodelijst.
// UitvoerderSpecialisme: SpecialismeAGBCodelijst.
// VerrichtingCategorie: VerrichtingCategorieCodelijst.



// ============================================
// MAPPING 1: Herkomst - SIM CSV
// ============================================

Mapping: SIMVerrichtingFromSIMCSV
Id: SIMVerrichtingFromSIMCSV
Title: "SIM CSV"
Description: "Herleidbaarheid van SIMVerrichting-elementen naar de oorspronkelijke SIM CSV-attributen."
Source: SIMVerrichting
Target: "urn:santeon:sim:csv"

// IBD-scope
* VerrichtingID -> "Verrichting;VerrichtingID"
* ContactID -> "Verrichting;ContactID"
* OperatieID -> "Verrichting;OperatieID"
* SubtrajectNummer -> "Verrichting;SubtrajectNr"
* PatientIdentificatienummer -> "Verrichting;Identificatienummer"
// VerrichtingType: NZa, SNOMED CT en DHD vormen afzonderlijke Coding-groepen binnen hetzelfde CodeableConcept, uitsluitend bij semantische overeenstemming.
// De velden worden per codestelsel gegroepeerd. Dit is een bronmapping, geen uitvoerbare transformatie naar de coding-array.
* VerrichtingType.coding.system -> "Verrichting;VerrichtingTypeCodeNZaSysteem, Verrichting;VerrichtingTypeCodeSnomedCTSyst, Verrichting;VerrichtingTypeCodeDHDSysteem"
* VerrichtingType.coding.code -> "Verrichting;VerrichtingTypeCodeNZa, Verrichting;VerrichtingTypeCodeSnomedCT, Verrichting;VerrichtingTypeCodeDHD"
* VerrichtingType.coding.display -> "Verrichting;VerrichtingTypeOmschrijvingNZa, Verrichting;VerrichtingTypeOmschrijvingSnomedCT, Verrichting;VerrichtingTypeOmschrijvingDHD"
* VerrichtingAantal -> "Verrichting;VerrichtingAantal"
* VerrichtingStartDatum -> "Verrichting;VerrichtingStartDatum"
* AanvragerSpecialisme.coding.system -> "Verrichting;Aanvrager_SpecialismeSysteem"
* AanvragerSpecialisme.coding.code -> "Verrichting;Aanvrager_Specialisme"
* UitvoerderSpecialisme.coding.system -> "Verrichting;Uitvoerder_SpecialismeSysteem"
* UitvoerderSpecialisme.coding.code -> "Verrichting;Uitvoerder_Specialisme"
* VerrichtingCategorie.coding.system -> "Verrichting;VerrichtingCategorieSysteem"
* VerrichtingCategorie.coding.code -> "Verrichting;VerrichtingCategorieCode"
* VerrichtingCategorie.coding.display -> "Verrichting;VerrichtingCategorieOmschrijving"

// Buiten IBD-scope - nog niet meegenomen
// * VerrichtingEindDatum -> "Verrichting;VerrichtingEindDatum"


// ============================================
// MAPPING 2: Semantische delta-analyse - SIMVerrichting naar Nictiz GBB-afsprakenmodel
// ============================================

Mapping: SIMVerrichtingToGBB
Id: SIMVerrichtingToGBB
Title: "Nictiz GBB-afsprakenmodel"
Description: "Semantische vergelijking tussen SIMVerrichting en Nictiz GBB Procedure (referentieversie 1.0.0-alpha.2). Legt per element binnen de IBD-scope de uitkomst van de vergelijking vast. Equivalent betekent dat het element is beoordeeld en er geen relevant semantisch verschil is vastgesteld. 🟡 Verschil vastgesteld of nader te beoordelen."
Source: SIMVerrichting
Target: "http://ig.nictiz.nl/gbb/StructureDefinition/LogicalModelProcedure"

// IBD-scope
* VerrichtingID -> "Procedure.feederAuditOriginatingSystemItemIds" "🟡 Verschil in cardinaliteit: SIM 1..1, GBB 0..*."
* PatientIdentificatienummer -> "Procedure.subject.patient.patientIdentificationNumber" "🟡 Nog te beoordelen: SIM legt het identificatienummer direct vast; GBB koppelt via GBB Patient."
* VerrichtingType -> "Procedure.procedureName" "🟡 Verschil in terminologiebinding: SIM gebruikt NZa, SNOMED CT en/of DHD; GBB gebruikt een required SNOMED CT ValueSet."
* VerrichtingStartDatum -> "Procedure.startDateTime" "Equivalent"
* VerrichtingCategorie -> "Procedure.procedureType" "🟡 Verschil in terminologiebinding: SIM gebruikt lokale zorgprofielklassen (25 codes); GBB gebruikt een required SNOMED CT ValueSet (3 codes). Geen directe één-op-één-equivalentie."
// Geen directe GBB-equivalent vastgesteld: ContactID, OperatieID, SubtrajectNummer, VerrichtingAantal, AanvragerSpecialisme (SIM-specifiek) en UitvoerderSpecialisme.
// GBB-elementen zonder direct SIM-equivalent: currentActionState (1..1) en dateAndTimeActionstepPerformed (1..1).

// Buiten IBD-scope - nog niet meegenomen
// * VerrichtingEindDatum -> "Procedure.endDateTime"


// ============================================
// MAPPING 3: Technische vertaling, harmonisatie en conversie - SIMVerrichting naar ProcedureSan
// De omgekeerde mapping (ProcedureSan naar SIMVerrichting) wordt automatisch gegenereerd.
// ============================================

Mapping: SIMVerrichtingToFHIR
Id: SIMVerrichtingToFHIR
Title: "FHIR-mapping naar ProcedureSan"
Description: "Mapping van SIMVerrichting-elementen naar het Santeon FHIR-profiel ProcedureSan, inclusief harmonisatie met FHIR-standaarden en toelichting op benodigde conversies."
Source: SIMVerrichting
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/procedure-san"

// IBD-scope
* VerrichtingID -> "Procedure.identifier" "Direct: samengestelde identifier (system|value); het waardedeel wordt gehasht volgens de ETL-specificatie."
* PatientIdentificatienummer -> "Procedure.subject" "Structuurconversie: Patient-reference op basis van identificatienummer."
* ContactID -> "Procedure.encounter" "Structuurconversie: Encounter-reference op basis van ContactID."
* VerrichtingType -> "Procedure.code" "Direct: NZa-, SNOMED CT- en DHD-coderingen worden alleen gecombineerd wanneer zij hetzelfde concept vertegenwoordigen. Verschil in cardinaliteit: VerrichtingType is in het SIM 0..1, maar ProcedureSan erft Procedure.code 1..1 van nl-core-Procedure; een verrichting zonder VerrichtingType kan niet als ProcedureSan worden aangeleverd. De IBD-aanlevering selecteert al op code, zodat daar geen verrichtingen wegvallen."
// TODO: Verifieer in de HIPS-transformatie of procedureperformedstart
// voor de IBD-aanlevering altijd Procedure.performedPeriod.start vult,
// zoals K3.6.3 verwacht. Bevestig ook het brondatatype en de dateshift.
* VerrichtingStartDatum -> "Procedure.performedPeriod.start" "Structuurconversie: bij een verrichting met tijdsduur performedPeriod.start, anders performedDateTime; dateshift volgens afzonderlijke transformatiespecificatie."
* VerrichtingCategorie -> "Procedure.category" "Direct: SIM CodeableConcept wordt overgenomen als FHIR CodeableConcept."

// Buiten IBD-scope - nog niet meegenomen
// * VerrichtingEindDatum -> "Procedure.performedPeriod.end"


// ============================================
// Status en openstaande controles
// ============================================

// Status: In progress
// TODO: Procedure.status is verplicht in FHIR R4 en ontbreekt in de SIM-bron.
// TODO: Besluit nodig over verrichtingen zonder VerrichtingType buiten de IBD-aanlevering
//   (niet aanleveren, of code vullen met bijvoorbeeld een data-absent-reason); ProcedureSan vereist code 1..1.
// TODO: FHIR-mapping uitwerken voor VerrichtingAantal, OperatieID, SubtrajectNummer, AanvragerSpecialisme en UitvoerderSpecialisme.
// TODO: GBB-delta beoordelen voor PatientIdentificatienummer (koppeling via GBB Patient).
// TODO: Terminologie voor VerrichtingType, AanvragerSpecialisme, UitvoerderSpecialisme en VerrichtingCategorie beoordelen (zie Terminologie).
