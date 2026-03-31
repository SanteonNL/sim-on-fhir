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
  - subject.identifier → hash
  - period.start       → dateshift
  - period.end         → dateshift
"""

* ^mapping[+].identity = "luscii-careplan-sql"
* ^mapping[=].name = "SQL Data Mapping (luscii_careplan.sql)"
* ^mapping[=].uri = "urn:santeon:mapping:luscii-careplan-sql"
* ^mapping[=].comment = "Maps CarePlan elements to luscii_careplan.sql"

* identifier ^mapping[0].identity = "luscii-careplan-sql"
* identifier ^mapping[0].map = "luscii_patients.Patient_Number
luscii_programs.name
luscii_patientsprogramshistory.createdAt"
* identifier ^mapping[0].comment = "Composite identifier from patient number, program, and enrollment date"

* category ^mapping[0].identity = "luscii-careplan-sql"
* category ^mapping[0].map = "luscii_programs.name"
* category ^mapping[0].comment = "Program name serves as the category code"

* category.coding ^mapping[0].identity = "luscii-careplan-sql"
* category.coding ^mapping[0].map = "luscii_programs.name"
* category.coding ^mapping[0].comment = "Program name serves as the category code"

* category.coding.code ^mapping[0].identity = "luscii-careplan-sql"
* category.coding.code ^mapping[0].map = "luscii_programs.name"
* category.coding.code ^mapping[0].comment = "Program name serves as the category code"

* subject ^mapping[0].identity = "luscii-careplan-sql"
* subject ^mapping[0].map = "luscii_patients.Patient_Number
luscii_programs.name
luscii_patientsprogramshistory.createdAt"
* subject ^mapping[0].comment = "Composite identifier from patient number, program, and enrollment date"

* period ^mapping[0].identity = "luscii-careplan-sql"
* period ^mapping[0].map = "luscii_patientsprogramshistory.createdAt
luscii_usersStatusChangeReasons.processedAt
luscii_patientsprogramshistory.createdAt"
* period ^mapping[0].comment = "Program enrollment date formatted as ISO 8601 UTC | Finds the earliest 'stopped' status change after program enrollment, but not beyond the next program enrollment. Returns NULL if no stop event exists (care is ongoing). Temporal constraints prevent overlapping periods."

* period.start ^mapping[0].identity = "luscii-careplan-sql"
* period.start ^mapping[0].map = "luscii_patientsprogramshistory.createdAt"
* period.start ^mapping[0].comment = "Program enrollment date formatted as ISO 8601 UTC"

* period.end ^mapping[0].identity = "luscii-careplan-sql"
* period.end ^mapping[0].map = "luscii_usersStatusChangeReasons.processedAt
luscii_patientsprogramshistory.createdAt"
* period.end ^mapping[0].comment = "Finds the earliest 'stopped' status change after program enrollment, but not beyond the next program enrollment. Returns NULL if no stop event exists (care is ongoing). Temporal constraints prevent overlapping periods."

* ^mapping[+].identity = "pj-careplan-sql"
* ^mapping[=].name = "SQL Data Mapping (pj_careplan.sql)"
* ^mapping[=].uri = "urn:santeon:mapping:pj-careplan-sql"
* ^mapping[=].comment = "Maps CarePlan elements to pj_careplan.sql"

* identifier ^mapping[1].identity = "pj-careplan-sql"
* identifier ^mapping[1].map = "pj_patients.patient_ref
pj_programmes.programme_code
pj_enrolments.enrolment_date"

* category ^mapping[1].identity = "pj-careplan-sql"
* category ^mapping[1].map = "pj_programmes.programme_code"

* category.coding ^mapping[1].identity = "pj-careplan-sql"
* category.coding ^mapping[1].map = "pj_programmes.programme_code"

* category.coding.code ^mapping[1].identity = "pj-careplan-sql"
* category.coding.code ^mapping[1].map = "pj_programmes.programme_code"

* subject ^mapping[1].identity = "pj-careplan-sql"
* subject ^mapping[1].map = "pj_patients.patient_ref
pj_programmes.programme_code
pj_enrolments.enrolment_date"

* period ^mapping[1].identity = "pj-careplan-sql"
* period ^mapping[1].map = "pj_enrolments.enrolment_date
pj_enrolments.discharge_date"

* period.start ^mapping[1].identity = "pj-careplan-sql"
* period.start ^mapping[1].map = "pj_enrolments.enrolment_date"

* period.end ^mapping[1].identity = "pj-careplan-sql"
* period.end ^mapping[1].map = "pj_enrolments.discharge_date"

// -----------------------------------------------------------
// Suppress meta/narrative overhead
// -----------------------------------------------------------

* meta 0..0
* implicitRules 0..0
* language 0..0
* text 0..0
* contained 0..0
* extension 0..0
* modifierExtension 0..0

// ============================================================
// [1] identifier — not used
// ============================================================
* identifier 0..0

// ============================================================
// [2] status  (required=true, base 1..1 — unchanged)
// draft | active | on-hold | revoked | completed | entered-in-error | unknown
// ============================================================


// ============================================================
// [3] intent  (required=true, base 1..1 — unchanged)
// proposal | plan | order | option
// ============================================================


// ============================================================
// [4-6] category  (required=true, exactly one Santeon category)
// ============================================================
* category 1..1
* category from http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.124.11.140--20240925070229 (required)
* category ^binding.description = "Zorgplan categorieën voor het thuismonitoringprogramma Zorg Bij Jou."

* category.id 0..0
* category.text 0..0
* category.coding 1..*
* category.coding.id 0..0
* category.coding.version 0..0
* category.coding.userSelected 0..0
* category.coding.system  1..1
* category.coding.code    1..1
* category.coding.display 1..1

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
* period.id 0..0
* period.start 1..1
* period.end   1..1

// ============================================================
// [10] created — not used
// ============================================================
* created 0..0

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


