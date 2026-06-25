Profile: PatientSan
Parent: Patient
Id: PatientSan
Title: "Santeon Patient"
Description: """
**De-identificatie:**

- identifier → hashed
- birthDate →  dateshift
"""

// Meta overhead
* meta 0..0
* implicitRules 0..0
* language 0..0
* text 0..0
* contained 0..0
* extension 0..0
* modifierExtension 0..0

// identifier
* identifier 1..1
* identifier.use 0..0
* identifier.value 1..1

// birthDate
* birthDate 1..1

// Laat alle andere elementen weg
* active 0..0
* name 0..0
* telecom 0..0
* gender 0..0
* deceased[x] 0..0
* address 0..0
* maritalStatus 0..0
* multipleBirth[x] 0..0
* photo 0..0
* contact 0..0
* communication 0..0
* generalPractitioner 0..0
* managingOrganization 0..0
* link 0..0

// mappings SIM -> FHIR
* ^mapping[+].identity = "sim"
* ^mapping[=].name = "Santeon Informatie Model"
* ^mapping[=].comment = "Mapping van Patient elementen naar SIM"

* identifier.value ^mapping[0].identity = "sim"
* identifier.value ^mapping[0].map = "Patient;Identificatienummer"
* identifier.value ^mapping[0].comment = "hashed"

* birthDate ^mapping[0].identity = "sim"
* birthDate ^mapping[0].map = "Patient;Geboortedatum"
* birthDate ^mapping[0].comment = "dateshift"
