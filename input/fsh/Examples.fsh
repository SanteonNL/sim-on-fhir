// ============================================================
// Example: Santeon CarePlan instance (CKD use case)
// Illustrates the CKD conservative plan proposal scenario
// from the CarePlanCreated field description.
// ============================================================

Instance: SanteonCarePlanExample
InstanceOf: SanteonCarePlan
Title: "Voorbeeld Santeon CarePlan (CKD conservatief)"
Description: "Minimaal CarePlan conform het SanteonCarePlan profiel. CKD-scenario: conservatief traject voorgesteld."
Usage: #example

* identifier.system = "https://ig.santeon.nl/fhir/NamingSystem/careplan-id"
* identifier.value  = "CP-HASH-A3F9B2"

* status = #draft
* intent = #proposal

* category[santeonCategory].coding.system  = "https://ig.santeon.nl/careplan/CodeSystem/santeon-careplan-category-cs"
* category[santeonCategory].coding.code    = #ckd
* category[santeonCategory].coding.display = "Chronische Nierschade"

* subject = Reference(ExamplePatient)

* period.start = "2025-01-15"
* period.end   = "2025-12-31"

* created = "2025-01-15"

// -----------------------------------------------------------
// Referenced patient (inline example — identifier hashed)
// -----------------------------------------------------------

Instance: ExamplePatient
InstanceOf: Patient
Title: "Voorbeeld Patiënt"
Description: "Minimale Patient resource voor gebruik in het CarePlan voorbeeld."
Usage: #example

* identifier.system = "https://ig.santeon.nl/fhir/NamingSystem/patient-id"
* identifier.value  = "HASH-7F2A91C4"
