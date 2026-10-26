Logical: SIMPatient
Parent: Base
Id: SIMPatient
Title: "Patiënt (model)"
Description: "Generiek logisch model voor patiëntgegevens binnen het Santeon Informatiemodel."

* Identificatienummer 1..1 Identifier "Identificatienummer van de patiënt"
* Geslacht 0..1 CodeableConcept "Administratief geslacht van de patiënt"
* Land 0..1 Coding "Land waar het adres zich bevindt"
* Geboortedatum 1..1 date "Geboortedatum van de patiënt"
* DatumOverlijden 0..1 dateTime "Datum waarop de patiënt is overleden"
* DatumCheckStatusOverlijden 0..1 dateTime "Datum waarop de overlijdensstatus is gecontroleerd"

* GerelateerdePersoon 0..* BackboneElement "Gerelateerde persoon"
* GerelateerdePersoon.Identificatienummer 0..1 Identifier "Identificatienummer van de gerelateerde persoon"
* GerelateerdePersoon.Relatie 0..1 CodeableConcept "Relatie tot de patiënt"

* Naam 0..1 BackboneElement "Naam van de patiënt"
* Naam.Initialen 0..1 string "Initialen van de patiënt"
* Naam.Achternaam 0..1 string "Achternaam van de patiënt"

* Geslacht from http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.40.2.17.1.8--20200901000000 (extensible)
* Land from http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.40.2.20.5.2--20200901000000 (extensible)
* GerelateerdePersoon.Relatie from http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.40.2.3.1.1--20200901000000 (extensible)


Mapping: SIMPatientFromSIMCSV
Id: SIMPatientFromSIMCSV
Title: "SIM CSV naar SIM Patient"
Source: SIMPatient
Target: "SIM CSV"

* Identificatienummer -> "Patient;Identificatienummer"
* Geslacht -> "Patient;GeslachtCodeSysteem, Patient;GeslachtCode, Patient;GeslachtOmschrijving"
* Land -> "Patient;LandSysteem, Patient;Land"
* Geboortedatum -> "Patient;Geboortedatum"
* DatumOverlijden -> "Patient;DatumOverlijden"
* DatumCheckStatusOverlijden -> "Patient;DatumCheckStatusOverlijden"
* GerelateerdePersoon.Identificatienummer -> "Patient;GerelateerdPersoonID"
* GerelateerdePersoon.Relatie -> "Patient;GerelateerdeRelatieSysteem, Patient;GerelateerdeRelatie"
* Naam.Initialen -> "Patient;PatientNameInitials"
* Naam.Achternaam -> "Patient;PatientNameFamilyName"


Mapping: SIMPatientToFHIR
Id: SIMPatientToFHIR
Title: "SIM Patient naar FHIR"
Source: SIMPatient
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/patient-san"

* Identificatienummer -> "Patient.identifier"
* Geboortedatum -> "Patient.birthDate"
* Geslacht -> "Patient.gender"
* Land -> "Patient.address.country" "ISO 3166-code naar string"
* DatumOverlijden -> "Patient.deceasedDateTime"
* Naam.Initialen -> "Patient.name.given"
* Naam.Achternaam -> "Patient.name.family"


Mapping: SIMPatientRelatedPersonToFHIR
Id: SIMPatientRelatedPersonToFHIR
Title: "SIM Patient gerelateerde persoon naar FHIR"
Source: SIMPatient
Target: "http://hl7.org/fhir/StructureDefinition/RelatedPerson"

* GerelateerdePersoon.Identificatienummer -> "RelatedPerson.identifier"
* GerelateerdePersoon.Relatie -> "RelatedPerson.relationship"

