// ============================================================
// Santeon CarePlan Profile
// IG:     santeon.careplan.ig
// Base:   http://hl7.org/fhir/StructureDefinition/CarePlan (R4)
//
// Generated from data dictionary (CarePlan fields spec).
// De-identification notes are in comments per element.
// ============================================================

Profile: SanteonCarePlan
Parent: CarePlan
Id: santeon-careplan
Title: "Santeon CarePlan"
Description: """
  Santeon-internal CarePlan profile derived from base FHIR R4.
  Only the elements present in the Santeon data dictionary are
  exposed; all other optional base elements are prohibited (0..0).

  De-identification strategy per element:
  - identifier         → hash
  - subject.identifier → hash
  - period.start       → dateshift
  - period.end         → dateshift
  - created            → dateshift
"""

// -----------------------------------------------------------
// Suppress meta/narrative overhead
// -----------------------------------------------------------
* text 0..0
* contained 0..0
* extension 0..0
* modifierExtension 0..0

// ============================================================
// [1] identifier  (PK=true, required=false, deident=hash)
// Business identifiers assigned by performer or other systems.
// Value will be hashed during de-identification.
// ============================================================
* identifier 0..*
* identifier.use 0..0
* identifier.type 0..0
* identifier.period 0..0
* identifier.assigner 0..0
* identifier.system 1..1
* identifier.value  1..1

// ============================================================
// [2] status  (required=true, base 1..1 — unchanged)
// draft | active | on-hold | revoked | completed | entered-in-error | unknown
// ============================================================

// ============================================================
// [3] intent  (required=true, base 1..1 — unchanged)
// proposal | plan | order | option
// ============================================================

// ============================================================
// [4-6] category  (required=true → min 1)
// Sliced to require at least one coding with system+code+display.
// ============================================================
* category 1..*
* category ^slicing.discriminator.type = #pattern
* category ^slicing.discriminator.path = "coding.system"
* category ^slicing.rules = #open
* category ^slicing.description = "Slice on category coding system"

* category contains santeonCategory 1..1
* category[santeonCategory] from http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.124.11.140--20240925070229 (extensible)
* category[santeonCategory].coding 1..*
* category[santeonCategory].coding.system  1..1
* category[santeonCategory].coding.code    1..1
* category[santeonCategory].coding.display 1..1

// ============================================================
// [7] subject  (base 1..1, deident=hash on Patient.identifier)
// Restricted to Patient reference only.
// ============================================================
* subject only Reference(Patient)

// ============================================================
// [8-9] period  (required=true, deident=dateshift on both dates)
// period.start = inclusiedatum
// ============================================================
* period 1..1
* period.start 1..1
* period.end   1..1

// ============================================================
// [10] created  (required=true, deident=dateshift)
// System-generated record creation timestamp.
// In CKD context: marks the moment a conservative plan was proposed.
// ============================================================
* created 1..1

// -----------------------------------------------------------
// Suppress all other optional CarePlan elements
// -----------------------------------------------------------
* instantiatesCanonical 0..0
* instantiatesUri 0..0
* basedOn 0..0
* replaces 0..0
* partOf 0..0
* title 0..0
* description 0..0
* encounter 0..0
* author 0..0
* contributor 0..0
* careTeam 0..0
* addresses 0..0
* supportingInfo 0..0
* goal 0..0
* activity 0..0
* note 0..0


