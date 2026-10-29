
Logical: SIMVerrichting
Parent: Base
// Parent: GBB - nog te beoordelen
Id: SIMVerrichting
Title: "Verrichting (model)"
Description: "Generiek logisch model voor een verrichting binnen het Santeon Informatiemodel."

// IBD
* VerrichtingID 1..1 Identifier "Uniek ID (key) binnen patiënt per verrichting"
* ContactID 0..1 Identifier "Uniek ID (key) binnen patiënt per contact"
* OperatieID 0..1 Identifier "Uniek ID (key) binnen patiënt per OK"
* SubtrajectNummer 0..1 Identifier "Identificatienummer van het DBC-subtraject"
* PatientIdentificatienummer 1..1 Identifier "Identificatienummer van de patiënt"

* VerrichtingType 0..1 CodeableConcept "Type verrichting"
* VerrichtingType ^definition = "Type verrichting, gecodeerd met NZa, SNOMED CT en/of DHD. Meerdere coderingen worden alleen gecombineerd wanneer zij hetzelfde verrichtingsconcept vertegenwoordigen."

* VerrichtingAantal 0..1 integer "Aantal verrichtingen"
* VerrichtingStartDatum 0..1 dateTime "Datum en eventueel tijd waarop de verrichting is gestart"

* AanvragerSpecialisme 0..1 Coding "Medisch specialisme dat de verrichting heeft aangevraagd"
* UitvoerderSpecialisme 0..1 Coding "Medisch specialisme dat de verrichting heeft uitgevoerd"

* VerrichtingCategorie 0..1 CodeableConcept "Duiding tot welke zorgprofielklasse een verrichting behoort"

// Overig van model Verrichting
// * VerrichtingEindDatum 0..1 dateTime "Datum en eventueel tijd waarop de verrichting is beëindigd"

// Terminologie nog te beoordelen:
// VerrichtingType: NZa, SNOMED CT, DHD
// AanvragerSpecialisme: SpecialismeAGBCodelijst
// UitvoerderSpecialisme: SpecialismeAGBCodelijst
// VerrichtingCategorie: VerrichtingCategorieCodelijst


Mapping: SIMVerrichtingFromSIMCSV
Id: SIMVerrichtingFromSIMCSV
Title: "SIM CSV"
Source: SIMVerrichting
Target: "urn:santeon:sim:csv"

// Identificatie en relaties
* VerrichtingID -> "Verrichting;VerrichtingID"
* ContactID -> "Verrichting;ContactID"
* OperatieID -> "Verrichting;OperatieID"
* SubtrajectNummer -> "Verrichting;SubtrajectNr"
* PatientIdentificatienummer -> "Verrichting;Identificatienummer"

// VerrichtingType
// NZa, SNOMED CT en DHD vormen afzonderlijke
// Coding-groepen binnen hetzelfde CodeableConcept,
// uitsluitend bij semantische overeenstemming.
//
// De velden worden per codestelsel gegroepeerd.
// Dit is een bronmapping, geen uitvoerbare
// transformatie naar de coding-array.

* VerrichtingType.coding.system -> "Verrichting;VerrichtingTypeCodeNZaSysteem, Verrichting;VerrichtingTypeCodeSnomedCTSysteem, Verrichting;VerrichtingTypeCodeDHDSysteem"
* VerrichtingType.coding.code -> "Verrichting;VerrichtingTypeCodeNZa, Verrichting;VerrichtingTypeCodeSnomedCT, Verrichting;VerrichtingTypeCodeDHD"
* VerrichtingType.coding.display -> "Verrichting;VerrichtingTypeOmschrijvingNZa, Verrichting;VerrichtingTypeOmschrijvingSnomedCT, Verrichting;VerrichtingTypeOmschrijvingDHD"

// Aantal en datum
* VerrichtingAantal -> "Verrichting;VerrichtingAantal"
* VerrichtingStartDatum -> "Verrichting;VerrichtingStartDatum"

// Specialismen
* AanvragerSpecialisme.code -> "Verrichting;Aanvrager_Specialisme"
* UitvoerderSpecialisme.code -> "Verrichting;Uitvoerder_Specialisme"

// VerrichtingCategorie
* VerrichtingCategorie.coding.code -> "Verrichting;VerrichtingCategorieCode"

// Overig van model Verrichting
// * VerrichtingEindDatum -> "Verrichting;VerrichtingEindDatum"


Mapping: SIMVerrichtingToFHIR
Id: SIMVerrichtingToFHIR
Title: "SIMVerrichting naar FHIR"
Source: SIMVerrichting
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/procedure-san"

// Identificatie en relaties
* VerrichtingID -> "Procedure.identifier"
* PatientIdentificatienummer -> "Procedure.subject" "Patient-reference op basis van identificatienummer."
* ContactID -> "Procedure.encounter" "Encounter-reference op basis van ContactID."

// Verrichting
* VerrichtingType -> "Procedure.code" "Coderingen combineren wanneer zij hetzelfde concept vertegenwoordigen."
* VerrichtingStartDatum -> "Procedure.performedPeriod.start" "Bij een verrichting met tijdsduur; anders performedDateTime."
* VerrichtingCategorie -> "Procedure.category"
// * VerrichtingEindDatum -> "Procedure.performedPeriod.end"


// Nog uit te werken:
// Procedure.status (FHIR verplicht)
// VerrichtingAantal
// OperatieID
// SubtrajectNummer
// AanvragerSpecialisme
// UitvoerderSpecialisme

Mapping: SIMVerrichtingToGBB
Id: SIMVerrichtingToGBB
Title: "SIMVerrichting naar Nictiz GBB Procedure"
Source: SIMVerrichting
Target: "http://ig.nictiz.nl/gbb/StructureDefinition/LogicalModelProcedure"

// Identificatie
* VerrichtingID -> "Procedure.feederAuditOriginatingSystemItemIds"

// Patient
* PatientIdentificatienummer -> "Procedure.subject.patient.patientIdentificationNumber" "Koppeling naar de patiëntidentificatie binnen GBB Patient."

// Verrichting
* VerrichtingType -> "Procedure.procedureName" "Alleen passende coderingen; GBB gebruikt een verplichte SNOMED-ValueSet."

* VerrichtingStartDatum -> "Procedure.startDateTime"

* VerrichtingCategorie -> "Procedure.procedureType" "Semantische en terminologische overeenstemming controleren."

// * VerrichtingEindDatum -> "Procedure.endDateTime"


// Nog te beoordelen:
// AanvragerSpecialisme:
// Geen directe tegenhanger vastgesteld.
//
// UitvoerderSpecialisme:
// Mogelijk via Procedure.participations.performer
// .practitionerRole, afhankelijk van de exacte
// representatie van het specialisme.
//
// ContactID, OperatieID en SubtrajectNummer:
// Geen directe GBB-mapping vastgesteld.
//
// VerrichtingAantal:
// Geen overeenkomstig element vastgesteld.
//
// GBB currentActionState en dateAndTimeActionstepPerformed
// zijn verplicht, maar ontbreken als afzonderlijke
// gegevens in de huidige SIMVerrichting-scope.
