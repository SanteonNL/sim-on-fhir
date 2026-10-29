Logical: SIMPatient
Parent: Base
// Parent: http://ig.nictiz.nl/gbb/StructureDefinition/LogicalModelPatient
// GBB wordt via een semantische mapping gekoppeld.
Id: SIMPatient
Title: "Patiënt (model)"
Description: "Generiek logisch model voor patiëntgegevens binnen het Santeon Informatiemodel."

// IBD
* Identificatienummer 1..1 Identifier "Identificatienummer van de patiënt"
* Geslacht 0..1 CodeableConcept "Administratief geslacht van de patiënt"
* Geboortedatum 0..1 date "Geboortedatum van de patiënt"
* DatumOverlijden 0..1 date "Datum waarop de patiënt is overleden"

// Overig van model Patient
// * Land 0..1 Coding "Land waar het adres zich bevindt"
// * DatumCheckStatusOverlijden 0..1 dateTime "Datum waarop de overlijdensstatus is gecontroleerd"

// * GerelateerdePersoon 0..* BackboneElement "Gerelateerde persoon"
// * GerelateerdePersoon.Identificatienummer 0..1 Identifier "Identificatienummer van de gerelateerde persoon"
// * GerelateerdePersoon.Relatie 0..1 CodeableConcept "Relatie tot de patiënt"

// * Naam 0..1 BackboneElement "Naam van de patiënt"
// * Naam.Initialen 0..1 string "Initialen van de patiënt"
// * Naam.Achternaam 0..1 string "Achternaam van de patiënt"

// Terminologie
* Geslacht from http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.40.2.17.1.8--20200901000000 (extensible)
// * Land from http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.40.2.20.5.2--20200901000000 (extensible)
// * GerelateerdePersoon.Relatie from http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.40.2.3.1.1--20200901000000 (extensible)


// Mapping vanuit SIM CSV
Mapping: SIMPatientFromSIMCSV
Id: SIMPatientFromSIMCSV
Title: "SIM CSV"
Source: SIMPatient
Target: "urn:santeon:sim:csv"

// IBD
* Identificatienummer -> "Patient;Identificatienummer"

* Geslacht.coding.system -> "Patient;GeslachtCodeSysteem"
* Geslacht.coding.code -> "Patient;GeslachtCode"
* Geslacht.coding.display -> "Patient;GeslachtOmschrijving"

* Geboortedatum -> "Patient;Geboortedatum"
* DatumOverlijden -> "Patient;DatumOverlijden"

// Overig van model Patient
// * Land.system -> "Patient;LandSysteem"
// * Land.code -> "Patient;Land"

// * DatumCheckStatusOverlijden -> "Patient;DatumCheckStatusOverlijden"

// * GerelateerdePersoon.Identificatienummer -> "Patient;GerelateerdPersoonID"
// * GerelateerdePersoon.Relatie.coding.system -> "Patient;GerelateerdeRelatieSysteem"
// * GerelateerdePersoon.Relatie.coding.code -> "Patient;GerelateerdeRelatie"

// * Naam.Initialen -> "Patient;PatientNameInitials"
// * Naam.Achternaam -> "Patient;PatientNameFamilyName"


// Mapping naar FHIR PatientSan
Mapping: SIMPatientToFHIR
Id: SIMPatientToFHIR
Title: "SIMPatient naar FHIR"
Source: SIMPatient
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/patient-san"

* Identificatienummer -> "Patient.identifier"
* Geslacht -> "Patient.gender" "Terminologiemapping naar FHIR AdministrativeGender"
* Geboortedatum -> "Patient.birthDate"
* DatumOverlijden -> "Patient.deceasedDateTime" "Datum uit SIM omzetten naar FHIR dateTime, met behoud van de oorspronkelijke datumprecisie."

// Overig van model Patient
// * Land -> "Patient.address.country" "Landcode naar string, bijvoorbeeld ISO 3166."
// * Naam.Initialen -> "Patient.name.given" "Initialen als given wanneer volledige voornamen ontbreken."
// * Naam.Achternaam -> "Patient.name.family"


// Mapping naar FHIR RelatedPerson
Mapping: SIMPatientRelatedPersonToFHIR
Id: SIMPatientRelatedPersonToFHIR
Title: "SIMPatient gerelateerde persoon naar FHIR"
Source: SIMPatient
Target: "http://hl7.org/fhir/StructureDefinition/RelatedPerson"

* Identificatienummer -> "RelatedPerson.patient" "Reference naar de patiënt waarop de relatie betrekking heeft."
// * GerelateerdePersoon.Identificatienummer -> "RelatedPerson.identifier"
// * GerelateerdePersoon.Relatie -> "RelatedPerson.relationship"



Mapping: SIMPatientToGBB
Id: SIMPatientToGBB
Title: "SIMPatient naar Nictiz GBB Patient"
Source: SIMPatient
Target: "http://ig.nictiz.nl/gbb/StructureDefinition/LogicalModelPatient"

* Identificatienummer -> "patientIdentificationNumber"
* Geslacht -> "gender"
* Geboortedatum -> "dateOfBirth" "Conversie van date naar dateTime zonder tijdstip te verzinnen."
* DatumOverlijden -> "deceasedDateTime"

// Nog te beoordelen:
// Naam -> nameInformation.humanName
// Land -> address
// GerelateerdePersoon -> contact of afzonderlijke RelatedPerson, afhankelijk van de betekenis.
