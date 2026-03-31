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
* category[santeonCategory] from SanteonCarePlanCategoryVS (extensible)
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
* period 0..1
* period.start 0..1
* period.end   0..1


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


// ============================================================
// ValueSet: Santeon CarePlan Category
// ============================================================

ValueSet: SanteonCarePlanCategoryVS
Id: santeon-careplan-category-vs
Title: "Santeon CarePlan Category"
Description: "Categories used for CarePlan classification within the Santeon hospital network."
* ^status = #draft
* ^experimental = true
* include codes from system http://snomed.info/sct
    where concept is-a #734163000
* http://loinc.org#38717-5
* http://loinc.org#38720-9
* include codes from system https://ig.santeon.nl/careplan/CodeSystem/santeon-careplan-category-cs


// ============================================================
// CodeSystem: Santeon local category codes
// ============================================================

CodeSystem: SanteonCarePlanCategoryCS
Id: santeon-careplan-category-cs
Title: "Santeon CarePlan Category Codes"
Description: "Local Santeon codes for CarePlan categories."
* ^url = "https://ig.santeon.nl/careplan/CodeSystem/santeon-careplan-category-cs"
* ^status = #draft
* ^experimental = true
* ^caseSensitive = true
* #oncology       "Oncologie"             "Zorgplannen voor oncologische behandelingen"
* #cardiology     "Cardiologie"           "Zorgplannen voor cardiale aandoeningen"
* #ckd            "Chronische Nierschade" "Conservatief/dialyse/transplantatie traject"
* #chronic        "Chronische Zorg"       "Langdurig zorgplan voor chronische aandoeningen"
* #palliative     "Palliatieve Zorg"      "Palliatief en end-of-life zorgplan"
* #rehabilitation "Revalidatie"           "Post-acuut revalidatiezorgplan"
