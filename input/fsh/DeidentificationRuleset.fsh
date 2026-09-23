// =====================================================================
// DeidentificationRuleset.fsh
// SIM on FHIR — de-identification Logical Model + standard ruleset
// FHIR R4 (4.0.1) · compile with SUSHI
// =====================================================================


// ---------------------------------------------------------------------
// CodeSystem — the de-identification actions
// ---------------------------------------------------------------------
CodeSystem: DeidentificationActionCS
Id: deidentification-action-cs
Title: "De-identification Action Code System"
Description: "The transformations a de-identification rule may apply to an element."
* ^status = #active
* ^experimental = false
* ^caseSensitive = true
* #none          "None"          "Export the element unchanged. Requires an exceptionReason."
* #hash          "Hash"          "Replace the value with an HMAC hash."
* #shift         "Shift"         "Move a date/dateTime by a per-patient offset."
* #clamp-age     "Clamp age"     "Bound the age implied by a birth date to a range."
* #first-of-month "First of month" "Floor a date to the first day of its month."


// ---------------------------------------------------------------------
// ValueSet — bound on rule.action (required)
// ---------------------------------------------------------------------
ValueSet: DeidentificationActionVS
Id: deidentification-action-vs
Title: "De-identification Action Value Set"
Description: "Allowed values for a de-identification rule's action."
* ^status = #active
* ^experimental = false
* include codes from system DeidentificationActionCS


// ---------------------------------------------------------------------
// Logical Model — the ruleset structure
// ---------------------------------------------------------------------
Logical: DeidentificationRuleset
Parent: Base
Id: deidentification-ruleset
Title: "De-identification Ruleset"
Description: """
A fully-resolved (effective) set of de-identification rules for one export.
It carries the actual rules in force, with no reference to a base ruleset
plus differences. Together with the export Parameters (which declare the
released elements via _elements), it fully describes how an export is
de-identified.
"""
* ^status = #draft

// --- ruleset-level metadata ---
* id 1..1 string "Ruleset identifier, e.g. 'santeon-default'."
* version 1..1 string "Semantic version of this ruleset."
* url 0..1 uri "Canonical URL, when this ruleset is a published resource."

// --- the rules ---
* rule 1..* BackboneElement "The de-identification rules in force for this export."
* rule.path 1..1 string
    "FHIRPath expression selecting the element(s) this rule applies to. A rule
     covers the named element and every element beneath it."
* rule.action 1..1 code "The transformation to apply."
* rule.action from DeidentificationActionVS (required)
* rule.priority 0..1 integer
    "Optional explicit ordering. For date elements the canonical order is
     shift, then first-of-month, then clamp-age; priority is for finer control."

// --- action parameters (constrained per action by the invariants below) ---
* rule.algorithm 0..1 code
    "Hash algorithm (hash only), e.g. 'hmac-sha256'."
* rule.propagateTo 0..* string
    "Reference paths rewritten with the same hash (hash on an id element),
     e.g. '*.subject', '*.patient'."
* rule.maxDays 0..1 integer
    "Maximum absolute date shift in days (shift only). Offset drawn from
     +/- maxDays, never zero."
* rule.minAge 0..1 integer "Lower age bound in years (clamp-age only)."
* rule.maxAge 0..1 integer "Upper age bound in years (clamp-age only)."
* rule.exceptionReason 0..1 string
    "Justification. Required for action 'none'; required when 'clamp-age'
     overrides the standard age range."

// --- per-action constraints ---
* rule obeys none-requires-reason
* rule obeys hash-requires-algorithm
* rule obeys shift-requires-maxdays
* rule obeys clampage-requires-bounds


// ---------------------------------------------------------------------
// Invariants — enforce valid parameter combinations per action
// ---------------------------------------------------------------------
Invariant: none-requires-reason
Description: "Action 'none' must carry an exceptionReason and no transform parameters."
Severity: #error
Expression: "action = 'none' implies (exceptionReason.exists() and algorithm.empty() and maxDays.empty() and minAge.empty() and maxAge.empty())"

Invariant: hash-requires-algorithm
Description: "Action 'hash' must specify algorithm and carry no date/age parameters."
Severity: #error
Expression: "action = 'hash' implies (algorithm.exists() and maxDays.empty() and minAge.empty() and maxAge.empty())"

Invariant: shift-requires-maxdays
Description: "Action 'shift' must specify maxDays and carry no hash/age parameters."
Severity: #error
Expression: "action = 'shift' implies (maxDays.exists() and algorithm.empty() and minAge.empty() and maxAge.empty())"

Invariant: clampage-requires-bounds
Description: "Action 'clamp-age' must specify both minAge and maxAge."
Severity: #error
Expression: "action = 'clamp-age' implies (minAge.exists() and maxAge.exists() and algorithm.empty() and maxDays.empty())"


// ---------------------------------------------------------------------
// Instance — the standard ruleset shipped with the IG
// ---------------------------------------------------------------------
Instance: santeon-default
InstanceOf: DeidentificationRuleset
Usage: #definition
Title: "Santeon Standard De-identification Ruleset"
Description: "The default ruleset every Santeon export inherits unless overridden."
* id = "santeon-default"
* version = "0.1.0"
* url = "https://ig.santeon.nl/sim-on-fhir/DeidentificationRuleset/santeon-default"

* rule[+].path = "Patient.id"
* rule[=].action = #hash
* rule[=].algorithm = #hmac-sha256
* rule[=].propagateTo[+] = "*.subject"
* rule[=].propagateTo[+] = "*.patient"

* rule[+].path = "Patient.identifier"
* rule[=].action = #hash
* rule[=].algorithm = #hmac-sha256

* rule[+].path = "**.ofType(date)"
* rule[=].action = #shift
* rule[=].maxDays = 15

* rule[+].path = "Patient.birthDate"
* rule[=].action = #first-of-month

* rule[+].path = "Patient.birthDate"
* rule[=].action = #clamp-age
* rule[=].minAge = 18
* rule[=].maxAge = 85
