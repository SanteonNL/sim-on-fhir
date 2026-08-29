// Generieke Verrichting
Profile: NlVerrichting
Parent: Procedure
Id: nl-verrichting
Title: "NL Verrichting"
Description: "Generiek herbruikbaar Procedure-profiel"

* status MS
* subject 1..1 MS
* subject only Reference(Patient)
* performed[x] 1..1 MS
* performedDateTime MS
* code 1..1 MS
* code.coding 1..* MS
* code.coding.system 1..1 MS
* code.coding.code 1..1 MS