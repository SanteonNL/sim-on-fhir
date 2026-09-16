Logical: SIMPatient
Id: SIMPatient
Title: "CLM SIMPatient"
Description: "Logisch model voor patiëntgegevens binnen het Santeon Informatiemodel."

* Identificatienummer 1..1 Identifier "Identificatienummer van de patiënt"
* Geboortedatum 1..1 date "Geboortedatum van de patiënt"

Mapping: SIMPatientFromSIMCSV
Id: SIMPatientFromSIMCSV
Title: "SIM CSV"
Source: SIMPatient
Target: "SIM CSV"

* Identificatienummer -> "Identificatienummer"
* Geboortedatum -> "Geboortedatum"

Mapping: SIMPatientToFHIR
Id: SIMPatientToFHIR
Title: "SIM Patient naar FHIR"
Source: SIMPatient
Target: "https://ig.santeon.nl/ibd/StructureDefinition/PatientSan"

* Identificatienummer -> "Patient.identifier"
* Geboortedatum -> "Patient.birthDate"