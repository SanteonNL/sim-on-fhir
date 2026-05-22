Profile: SanteonIBDCohort
Parent: Group
Id: santeon-ibd-cohort
Title: "Santeon IBD Cohort"
Description: "Defines a definitional cohort of IBD patients used for Santeon quality indicators."

* ^status = #draft

// -------------------------------------------------------------------
// Fixed cohort behavior
// -------------------------------------------------------------------

* type = #person
* actual = false

// -------------------------------------------------------------------
// Cohort criteria
// -------------------------------------------------------------------

* characteristic 1..*
* characteristic.code 1..1
* characteristic.value[x] 1..1

// Restrict characteristic values
* characteristic.value[x] only CodeableConcept or boolean or Quantity or Range

// -------------------------------------------------------------------
// Human-readable metadata
// -------------------------------------------------------------------

* name 1..1

// -------------------------------------------------------------------
// Cohort members are not explicitly enumerated
// -------------------------------------------------------------------

* member 0..0