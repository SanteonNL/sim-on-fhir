// ============================================================
// Santeon MeasureReport Profile
// IG:     santeon.ibd.ig
// Base:   http://hl7.org/fhir/StructureDefinition/MeasureReport (R4)
//
// Reports the result of a Measure evaluation (e.g. K3.6.3).
// ============================================================

Profile: SanteonMeasureReport
Parent: MeasureReport
Id: santeon-measurereport
Title: "Santeon MeasureReport"
Description: """
  Santeon-internal MeasureReport profile for reporting IBD quality
  indicator results (e.g. K3.6.3).

  Each MeasureReport captures the computed indicator values for a
  given reporting period, including numerator, denominator, and
  the resulting proportion.
"""

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
// [1] status  (required=true, base 1..1)
// complete | pending | error
// ============================================================


// ============================================================
// [2] type  (required=true)
// summary = aggregate indicator reporting (no patient-level data)
// individual = per-patient report (includes evaluatedResource)
// ============================================================
* type from http://hl7.org/fhir/ValueSet/measure-report-type (required)

// ============================================================
// [3] measure  (required=true)
// Reference to the Measure being reported
// ============================================================
* measure 1..1

// ============================================================
// [4] date  (required=true)
// When this report was generated
// ============================================================
* date 1..1

// ============================================================
// [5] period  (required=true)
// Reporting period
// ============================================================
* period 1..1
* period.start 1..1
* period.end 1..1

// ============================================================
// [6] group  (required=true)
// Indicator results
// ============================================================
* group 1..*
* group.code 1..1
* group.population 1..*
* group.population.code 1..1
* group.population.count 1..1
* group.measureScore 1..1

// ============================================================
// [7] subject  (conditional)
// For type=individual: required (reference to Patient)
// For type=summary: not used
// ============================================================
* subject only Reference(Patient)

// -----------------------------------------------------------
// Suppress other optional elements
// -----------------------------------------------------------
* reporter 0..0
* improvementNotation 0..0

// ============================================================
// [8] evaluatedResource  (optional)
// References to the actual Procedure and Observation resources
// that were evaluated for this report.
//
// For type=summary: typically not populated (aggregate counts)
// For type=individual: populated with the specific resources
//   - SanteonProcedure (scopie verrichtingen)
//   - SanteonObservation (calprotectine metingen)
// ============================================================
* evaluatedResource 0..*
