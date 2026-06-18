// Generieke DBC
Profile: NlDbc
Parent: EpisodeOfCare
Id: nl-dbc
Title: "NL DBC"
Description: "Generiek herbruikbaar EpisodeOfCare-profiel voor DBC"

* status MS
* period 1..1 MS
* period.start 1..1 MS
* type 1..* MS              // zorgtype
* type.coding.system 1..1 MS
* type.coding.code 1..1 MS
* diagnosis 1..* MS
* diagnosis.condition 1..1 MS

/* diagnosis.condition.coding 1..* MS
* diagnosis.condition.coding.system 1..1 MS
* diagnosis.condition.coding.code 1..1 MS
* patient 1..1 MS
* patient only Reference(Patient) */