// ============================================================
// Santeon IBD Measure — Indicator K3.6.3
// IG:     santeon.ibd.ig
// Base:   http://hl7.org/fhir/StructureDefinition/Measure (R4)
//
// Indicator: % scopieën waarbij ≤90 dagen voorafgaand aan de
//            scopie een calprotectine meting is gedaan.
//
// Replaces R-function: f_Doorlooptijd_meting_tot_verrichting
// ============================================================

Instance: SanteonIBDMeasureK363
InstanceOf: Measure
Title: "Santeon IBD Kwaliteitsindicator K3.6.3"
Description: """
  IBD kwaliteitsindicator K3.6.3: Percentage scopieën waarbij
  tot 90 dagen voorafgaand aan de scopie een calprotectine
  meting (fecaal calprotectine, LOINC 38445-3) is gedaan.

  Bron: HipsETL indicatoren zorgtrajecten 30_gold_ibd.r
  R-functie: f_Doorlooptijd_meting_tot_verrichting

  Populatie: Patiënten met een scopie (NZa codes: 034620,
  034686, 034690, 035582). De populatie kan optioneel worden
  ingeperkt via indicator K3.1.2 (f_Zorgactiviteit), maar
  dit is niet vereist voor de kernberekening.
"""
Usage: #definition

* url = "https://ig.santeon.nl/ibd/Measure/ibd-k363"
* name = "SanteonIBDMeasureK363"
* status = #active
* experimental = false
* date = "2026-04-28"
* publisher = "Santeon"

* title = "IBD Indicator K3.6.3 — Calprotectine vóór scopie"
* description = "Percentage scopieën waarbij tot 90 dagen voorafgaand aan de scopie een calprotectine meting is gedaan."

* subjectCodeableConcept = http://hl7.org/fhir/resource-types#Patient

// === Scoring: proportion (percentage) ===
* scoring = http://terminology.hl7.org/CodeSystem/measure-scoring#proportion "Proportion"
* type = http://terminology.hl7.org/CodeSystem/measure-type#process "Process"

// === Improvement notation: higher is better ===
* improvementNotation = http://terminology.hl7.org/CodeSystem/measure-improvement-notation#increase "Increased score indicates improvement"

// === Library: no CQL, logic described narratively ===
// The indicator logic is expressed in the group definitions below.
// Future iteration may add a CQL library for automated computation.

// ============================================================
// Group: K3.6.3 — Calprotectine binnen 90 dagen vóór scopie
// ============================================================
* group[+].code.coding.system = "https://ig.santeon.nl/ibd/CodeSystem/santeon-indicator"
* group[=].code.coding.code = #K3.6.3
* group[=].code.coding.display = "Calprotectine vóór scopie"
* group[=].code.text = "IBD Indicator K3.6.3"
* group[=].description = "Berekent het percentage scopie-verrichtingen waarvoor binnen 90 dagen voorafgaand een calprotectine meting is gedaan."

// --- Initial Population ---
* group[=].population[+].code = http://terminology.hl7.org/CodeSystem/measure-population#initial-population "Initial Population"
* group[=].population[=].description = """
  Alle patiënten met minimaal één scopie-verrichting.
  Procedure.code bevat NZa-code in {034620, 034686, 034690, 035582}.
  Conform profiel: SanteonProcedure (santeon-procedure).
  
  FHIR query equivalent:
  Procedure?code=urn:oid:2.16.840.1.113883.2.4.6.14|034620,
    urn:oid:2.16.840.1.113883.2.4.6.14|034686,
    urn:oid:2.16.840.1.113883.2.4.6.14|034690,
    urn:oid:2.16.840.1.113883.2.4.6.14|035582
  &status=completed
"""
* group[=].population[=].criteria.language = #text/plain
* group[=].population[=].criteria.expression = "Patients with at least one completed Procedure where code is in NZa {034620, 034686, 034690, 035582}"

// --- Denominator ---
* group[=].population[+].code = http://terminology.hl7.org/CodeSystem/measure-population#denominator "Denominator"
* group[=].population[=].description = """
  Alle scopie-verrichtingen (Procedure resources) met:
  - Procedure.code NZa in {034620, 034686, 034690, 035582}
  - Procedure.status = completed
  - Conform profiel: SanteonProcedure
  
  Eenheid: per verrichting (niet per patiënt).
"""
* group[=].population[=].criteria.language = #text/plain
* group[=].population[=].criteria.expression = "All completed Procedure resources where code is in NZa {034620, 034686, 034690, 035582}"

// --- Numerator ---
* group[=].population[+].code = http://terminology.hl7.org/CodeSystem/measure-population#numerator "Numerator"
* group[=].population[=].description = """
  Scopie-verrichtingen waarvoor geldt:
  Er bestaat een Observation (calprotectine) waarbij:
  - Observation.code = LOINC 38445-3 (Fecal calprotectin)
  - Observation.subject = dezelfde patiënt als Procedure.subject
  - Observation.effectiveDateTime valt in het venster:
      [Procedure.performedPeriod.start - 90 dagen,
       Procedure.performedPeriod.start]
  - Conform profiel: SanteonObservation
  
  R-equivalent: MetingTotVerrichting = TRUE, n_Window = 90
  
  FHIR query equivalent (per scopie):
  Observation?code=http://loinc.org|38445-3
    &patient=[Procedure.subject]
    &date=ge[Procedure.performedPeriod.start - 90d]
    &date=le[Procedure.performedPeriod.start]
"""
* group[=].population[=].criteria.language = #text/plain
* group[=].population[=].criteria.expression = "Denominator Procedures where there exists an Observation with code LOINC 38445-3 for the same patient, with effectiveDateTime within 90 days before Procedure.performedPeriod.start"

// ============================================================
// Stratifier: per specialisme (optioneel, voor rapportage)
// ============================================================
* group[=].stratifier[+].code.coding.system = "https://ig.santeon.nl/ibd/CodeSystem/santeon-stratifier"
* group[=].stratifier[=].code.coding.code = #performer-specialty
* group[=].stratifier[=].code.coding.display = "Uitvoerder specialisme"
* group[=].stratifier[=].description = "Stratificatie op basis van het specialisme van de uitvoerend arts (Procedure.performer.actor → Practitioner.qualification)"
* group[=].stratifier[=].criteria.language = #text/plain
* group[=].stratifier[=].criteria.expression = "Procedure.performer.actor.resolve().qualification.code"

// ============================================================
// Related artifacts: link to source profiles
// ============================================================
* relatedArtifact[+].type = #depends-on
* relatedArtifact[=].display = "SanteonObservation profiel (AlgemeneMeting/calprotectine)"
* relatedArtifact[=].resource = "https://ig.santeon.nl/ibd/StructureDefinition/santeon-observation"

* relatedArtifact[+].type = #depends-on
* relatedArtifact[=].display = "SanteonProcedure profiel (Verrichting/scopie)"
* relatedArtifact[=].resource = "https://ig.santeon.nl/ibd/StructureDefinition/santeon-procedure"
