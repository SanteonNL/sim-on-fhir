// ============================================================
// Santeon Observation Profile (AlgemeneMeting)
// IG:     santeon.ibd.ig
// Base:   http://hl7.org/fhir/StructureDefinition/Observation (R4)
// ZIB:    AlgemeneMeting-v3.0(2019NL)
//
// Used in IBD indicator K3.6.3: calprotectine measurement
// De-identification notes are in comments per element.
// ============================================================

Profile: SanteonObservation
Parent: Observation
Id: santeon-observation
Title: "Santeon Observation (AlgemeneMeting)"
Description: """
  Santeon-internal Observation profile derived from base FHIR R4,
  mapped to ZIB AlgemeneMeting-v3.0(2019NL).

  Used for general measurements including calprotectine (LOINC 38445-3)
  in the IBD use case (indicator K3.6.3).

  De-identification strategy per element:
  - subject.identifier → hash
  - effectiveDateTime  → dateshift
"""

// === SQL Data Source Mapping ===

* ^mapping[+].identity = "hips-observation-sql"
* ^mapping[=].name = "SQL Data Mapping (hips_observation.sql)"
* ^mapping[=].uri = "urn:santeon:mapping:hips-observation-sql"
* ^mapping[=].comment = "Maps Observation elements to hips_observation.sql"

* code ^mapping[0].identity = "hips-observation-sql"
* code ^mapping[0].map = "AlgemeneMeting.MetingNaamCode"
* code ^mapping[0].comment = "LOINC code for the measurement type (e.g. 38445-3 = Calprotectin)"

* code.coding ^mapping[0].identity = "hips-observation-sql"
* code.coding ^mapping[0].map = "AlgemeneMeting.MetingNaamCode"

* code.coding.code ^mapping[0].identity = "hips-observation-sql"
* code.coding.code ^mapping[0].map = "AlgemeneMeting.MetingNaamCode"
* code.coding.code ^mapping[0].comment = "LOINC code for the measurement type"

* subject ^mapping[0].identity = "hips-observation-sql"
* subject ^mapping[0].map = "AlgemeneMeting.Identificatienummer"
* subject ^mapping[0].comment = "Patient identifier (BSN), de-identified via hash"

* effective[x] ^mapping[0].identity = "hips-observation-sql"
* effective[x] ^mapping[0].map = "AlgemeneMeting.MetingDatumTijd"
* effective[x] ^mapping[0].comment = "Measurement date/time, de-identified via dateshift"

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
// [1] identifier  (optional, for observation-level ID)
// ============================================================
* identifier 0..*
* identifier.id 0..0
* identifier.use 0..0
* identifier.type 0..0
* identifier.period 0..0
* identifier.assigner 0..0

// ============================================================
// [2] status  (required=true, base 1..1 — unchanged)
// registered | preliminary | final | amended | corrected |
// cancelled | entered-in-error | unknown
// ============================================================


// ============================================================
// [3] code  (required=true, MetingNaamCode — LOINC)
// Maps to ZIB AlgemeneMeting MetingNaamCode
// ============================================================
* code 1..1
* code.id 0..0
* code.text 0..0
* code.coding 1..*
* code.coding.id 0..0
* code.coding.version 0..0
* code.coding.userSelected 0..0
* code.coding.system 1..1
* code.coding.code 1..1
* code.coding.display 0..1

// ============================================================
// [4] subject  (required=true, deident=hash on Patient.identifier)
// Maps to ZIB AlgemeneMeting Identificatienummer
// Restricted to Patient reference only.
// ============================================================
* subject 1..1
* subject only Reference(Patient)

// ============================================================
// [5] effectiveDateTime  (required=true, deident=dateshift)
// Maps to ZIB AlgemeneMeting MetingDatumTijd
// ============================================================
* effective[x] only dateTime
* effective[x] 1..1

// ============================================================
// [6] value[x]  (optional — measurement result value)
// Not mapped in K3.6.3 indicator but retained for completeness
// ============================================================

// -----------------------------------------------------------
// Suppress all other optional Observation elements
// -----------------------------------------------------------
* basedOn 0..0
* partOf 0..0
* category 0..0
* focus 0..0
* encounter 0..0
* issued 0..0
* performer 0..0
* dataAbsentReason 0..0
* interpretation 0..0
* note 0..0
* bodySite 0..0
* method 0..0
* specimen 0..0
* device 0..0
* referenceRange 0..0
* hasMember 0..0
* derivedFrom 0..0
* component 0..0
