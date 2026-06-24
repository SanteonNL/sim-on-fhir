Profile: ProcedureSan
Parent: Procedure
Id: ProcedureSan
Title: "Santeon Procedure (Verrichting)"

* identifier 1..1

* subject 1..1
* subject only Reference(PatientSan)

// * code 1..1

* code.coding 1..*
* code.coding.system 1..1
* code.coding.code 1..1
* code.coding.display 0..1