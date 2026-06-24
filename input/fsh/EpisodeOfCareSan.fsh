Profile: EpisodeOfCareSan
Parent: EpisodeOfCare
Id: EpisodeOfCareSan
Title: "Santeon EpisodeOfCare (DBC)"

* patient 1..1
* patient only Reference(PatientSan)

* status 1..1

* period 1..1
* period.start 1..1

* type.coding.system 1..1
* type.coding.code 1..1
* type.coding.display 0..1

* diagnosis 1..*
* diagnosis.condition 1..1

// verbieden 0..0