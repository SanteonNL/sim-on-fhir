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

// Identificatie
* identifier 1..1
* identifier.system 0..1
* identifier.value 1..1

// Administratief geslacht
* gender 0..1
* gender ^short = "Administratief geslacht van de patiënt"

// Geboortedatum
* birthDate 1..1

// Overlijdensinformatie
* deceased[x] 0..1

// Adresgegevens
* address 0..*
* address.country 0..1

// Naamgegevens
* name 0..*
* name.given 0..*
* name.family 0..1
