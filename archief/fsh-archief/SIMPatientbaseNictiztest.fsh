Logical: SIMPatientbaseNictiztest
Parent: http://ig.nictiz.nl/gbb/StructureDefinition/LogicalModelPatient
// Base
Id: SIMPatientbaseNictiztest
Title: "Patiënt (model) base nictiz patient"
Description: "Logisch model voor patiëntgegevens binnen het Santeon Informatiemodel."

* patientIdentificationNumber 1..1
* patientIdentificationNumber ^short = "Identificatienummer van de patiënt"
* dateOfBirth 1..1
* dateOfBirth ^short = "Geboortedatum van de patiënt"

Mapping: SIMPatientNictizFromSIMCSV
Id: SIMPatientNictizFromSIMCSV
Title: "SIM CSV"
Source: SIMPatientbaseNictiztest
Target: "SIM CSV"

* patientIdentificationNumber -> "Identificatienummer"
* dateOfBirth -> "Geboortedatum"

Mapping: SIMPatientNictizToFHIR
Id: SIMPatientNictizToFHIR
Title: "SIM Patient naar FHIR"
Source: SIMPatientbaseNictiztest
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/patient-san"

* patientIdentificationNumber -> "Patient.identifier"
* dateOfBirth -> "Patient.birthDate"