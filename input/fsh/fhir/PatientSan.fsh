Profile: PatientSan
Parent: Patient
Id: patient-san
Title: "Patient (SAN)"
Description: """
Generiek Santeon FHIR R4-profiel voor patiëntgegevens.

Gebaseerd op het Santeon Informatiemodel (SIMPatient).

De-identificatie wordt uitgevoerd volgens afzonderlijke
transformatiespecificaties en is geen inherente
eigenschap van dit profiel.
"""

// Meta overhead: conform huidige opzet van de SAN-profielen.
// Beoordeel deze beperkingen opnieuw wanneer meta.profile/securityLabel nodig is.
* meta 0..0
* implicitRules 0..0
* language 0..0
* text 0..0
* contained 0..0
* extension 0..0
* modifierExtension 0..0

// Identificatie (SIMPatient.Identificatienummer)
* identifier 1..1
* identifier.use 0..0
* identifier.value 1..1
// identifier.system blijft toegestaan; het bronveld bevat
// identifier_system|identifier_value als samengestelde representatie.

// Administratief geslacht (SIMPatient.Geslacht)
* gender 0..1
// Patient.gender is FHIR R4 code, geen CodeableConcept.
// SIM-codering moet naar AdministrativeGender worden vertaald.

// Geboortedatum (SIMPatient.Geboortedatum)
* birthDate 0..1

// Datum overlijden (SIMPatient.DatumOverlijden)
* deceased[x] only dateTime
// Een bronwaarde met alleen een datum mag als gedeeltelijke dateTime
// worden opgenomen; geen onbekend tijdstip aanvullen.

// Niet in de huidige IBD-scope; later heroverwegen voor generiek gebruik.
* active 0..0
* name 0..0
* telecom 0..0
* address 0..0
* maritalStatus 0..0
* multipleBirth[x] 0..0
* photo 0..0
* contact 0..0
* communication 0..0
* generalPractitioner 0..0
* managingOrganization 0..0
* link 0..0

// Gedetailleerde bronmapping: FHIR-profiel naar SIM CSV.
// Een FSH Mapping documenteert de relatie; voert geen ETL-transformatie uit.
Mapping: PatientSanFromSIMCSV
Id: sim
Title: "Santeon Informatiemodel (SIM CSV)"
Source: PatientSan
Target: "urn:santeon:sim:csv"

* identifier.system -> "Patient;Identificatienummer" "Systeemdeel van de samengestelde identifier."
* identifier.value -> "Patient;Identificatienummer" "Waardedeel van de samengestelde identifier; hashed."
* gender -> "Patient;GeslachtCodeSysteem, Patient;GeslachtCode, Patient;GeslachtOmschrijving" "Vertaling van SIM CodeableConcept naar FHIR AdministrativeGender vereist."
* birthDate -> "Patient;Geboortedatum" "Bron dateTime naar FHIR date, met geboortedatum_birthdateshift en zonder onbedoelde precisieverandering."
* deceasedDateTime -> "Patient;DatumOverlijden" "Bron date naar FHIR dateTime met dezelfde datumprecisie; dateshift."
