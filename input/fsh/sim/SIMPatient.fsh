Logical: SIMPatient
Parent: Base
Id: SIMPatient
Title: "Patiënt (model)"
Description: "Generiek logisch model voor patiëntgegevens binnen het Santeon Informatiemodel."

// IBD-scope
* Identificatienummer 1..* Identifier "Identificatienummer van de patiënt"
* Geslacht 0..1 CodeableConcept "Administratief geslacht van de patiënt"
* Geboortedatum 0..1 date "Geboortedatum van de patiënt"
* DatumOverlijden 0..1 date "Datum waarop de patiënt is overleden"

// Buiten IBD-scope - nog niet meegenomen
// * Land 0..1 Coding "Land waar het adres zich bevindt"
// * DatumCheckStatusOverlijden 0..1 dateTime "Datum waarop de overlijdensstatus is gecontroleerd"
// * GerelateerdePersoon 0..* BackboneElement "Gerelateerde persoon"
// * GerelateerdePersoon.Identificatienummer 0..1 Identifier "Identificatienummer van de gerelateerde persoon"
// * GerelateerdePersoon.Relatie 0..1 CodeableConcept "Relatie tot de patiënt"
// * Naam 0..1 BackboneElement "Naam van de patiënt"
// * Naam.Initialen 0..1 string "Initialen van de patiënt"
// * Naam.Achternaam 0..1 string "Achternaam van de patiënt"

// Terminologie
* Geslacht from http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.40.2.17.1.8--20200901000000 (required)
// * Land from http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.40.2.20.5.2--20200901000000 (extensible)
// * GerelateerdePersoon.Relatie from http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.40.2.3.1.1--20200901000000 (extensible)



// ============================================
// MAPPING 1: Herkomst - SIM CSV
// ============================================

Mapping: SIMPatientFromSIMCSV
Id: SIMPatientFromSIMCSV
Title: "SIM CSV"
Description: "Herleidbaarheid van SIMPatient-elementen naar de oorspronkelijke SIM CSV-attributen."
Source: SIMPatient
Target: "urn:santeon:sim:csv"

// IBD-scope
* Identificatienummer -> "Patient;Identificatienummer"
* Geslacht.coding.system -> "Patient;GeslachtCodeSysteem"
* Geslacht.coding.code -> "Patient;GeslachtCode"
* Geslacht.coding.display -> "Patient;GeslachtOmschrijving"
* Geboortedatum -> "Patient;Geboortedatum"
* DatumOverlijden -> "Patient;DatumOverlijden"

// Buiten IBD-scope - nog niet meegenomen
// * Land.system -> "Patient;LandSysteem"
// * Land.code -> "Patient;Land"
// * DatumCheckStatusOverlijden -> "Patient;DatumCheckStatusOverlijden"
// * GerelateerdePersoon.Identificatienummer -> "Patient;GerelateerdPersoonID"
// * GerelateerdePersoon.Relatie.coding.system -> "Patient;GerelateerdeRelatieSysteem"
// * GerelateerdePersoon.Relatie.coding.code -> "Patient;GerelateerdeRelatie"
// * Naam.Initialen -> "Patient;PatientNameInitials"
// * Naam.Achternaam -> "Patient;PatientNameFamilyName"


// ============================================
// MAPPING 2: Semantische delta-analyse - SIMPatient naar Nictiz GBB-afsprakenmodel
// ============================================

Mapping: SIMPatientToGBB
Id: SIMPatientToGBB
Title: "Nictiz GBB-afsprakenmodel"
Description: "Semantische vergelijking tussen SIMPatient en Nictiz GBB Patient (referentieversie 1.0.0-alpha.2). Legt per element binnen de IBD-scope de uitkomst van de vergelijking vast. Equivalent betekent dat het element is beoordeeld en er geen relevant semantisch verschil is vastgesteld. 🟡 Verschil vastgesteld of nader te beoordelen."
Source: SIMPatient
Target: "http://ig.nictiz.nl/gbb/StructureDefinition/LogicalModelPatient"

// IBD-scope
* Identificatienummer -> "Patient.patientIdentificationNumber" "Equivalent"
* Geslacht -> "Patient.gender" "🟡 Verschil in terminologiebinding: SIM gebruikt GeslachtCodelijst ZIB 2020; GBB gebruikt FHIR AdministrativeGender."
* Geboortedatum -> "Patient.dateOfBirth" "Semantisch compatibel op dagniveau: SIM gebruikt date; GBB gebruikt dateTime."
* DatumOverlijden -> "Patient.deceased[x]" "🟡 Verschil in representatie: SIM registreert uitsluitend de overlijdensdatum als date; GBB ondersteunt zowel dateTime als boolean."

// Buiten IBD-scope - nog te beoordelen
// * Naam -> 
// * Land -> 
// * GerelateerdePersoon -> 
// * DatumCheckStatusOverlijden -> 


// ============================================
// MAPPING 3: Technische vertaling, harmonisatie en conversie - SIMPatient naar PatientSan
// De omgekeerde mapping (PatientSan naar SIMPatient) wordt automatisch gegenereerd.
// ============================================

Mapping: SIMPatientToFHIR
Id: SIMPatientToFHIR
Title: "FHIR-mapping naar PatientSan"
Description: "Mapping van SIMPatient-elementen naar het Santeon FHIR-profiel PatientSan, inclusief harmonisatie met FHIR-standaarden en toelichting op benodigde conversies."
Source: SIMPatient
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/patient-san"

// IBD-scope
* Identificatienummer -> "Patient.identifier" "Direct: system en value worden behouden conform de afgesproken identificatiesystematiek."
* Geslacht -> "Patient.gender" "Terminologieconversie: SIM GeslachtCodelijst ZIB 2020 naar FHIR AdministrativeGender volgens de Nictiz-ConceptMap GeslachtCodelijst-to-AdministrativeGender (http://nictiz.nl/fhir/ConceptMap/GeslachtCodelijst-to-AdministrativeGender, zib2020, draft): M naar male, F naar female, UN naar other (narrower), UNK naar unknown; toepassing van deze ConceptMap in de ETL nog te bevestigen."
* Geboortedatum -> "Patient.birthDate" "Direct: SIM date wordt overgenomen als FHIR date."
* DatumOverlijden -> "Patient.deceasedDateTime" "Datatypeconversie: SIM date naar FHIR dateTime met dagprecisie; deceasedBoolean wordt vanuit dit SIM-element niet gevuld."


// ============================================
// Status en openstaande controles
// ============================================

// Status: Ready for validation
// TODO: Terminologieconversie naar AdministrativeGender valideren.
// TODO: Identificatiesystematiek voor Identifier.system valideren.