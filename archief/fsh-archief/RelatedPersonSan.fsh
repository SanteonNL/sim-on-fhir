Profile: RelatedPersonSan
Parent: RelatedPerson
Id: related-person-san
Title: "RelatedPerson (SAN)"
Description: "Generiek Santeon-profiel voor personen die gerelateerd zijn aan een patiënt."

* patient 1..1
* patient only Reference(PatientSan)

* identifier 0..*
* relationship 0..*
