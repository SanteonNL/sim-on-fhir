// ============================================================
// Examples for Santeon IBD IG
// ============================================================

// -----------------------------------------------------------
// Referenced patient (inline example — identifier hashed)
// -----------------------------------------------------------

Instance: ExamplePatient
InstanceOf: Patient
Title: "Voorbeeld Patiënt"
Description: "Minimale Patient resource voor gebruik in de IBD voorbeelden."
Usage: #example

* identifier.system = "https://ig.santeon.nl/fhir/NamingSystem/patient-id"
* identifier.value  = "HASH-7F2A91C4"

// ============================================================
// Example: Santeon Observation (Calprotectine meting)
// IBD indicator K3.6.3
// ============================================================

Instance: SanteonObservationExample
InstanceOf: SanteonObservation
Title: "Voorbeeld Santeon Observation (Calprotectine)"
Description: "Calprotectine meting conform het SanteonObservation profiel. IBD use case indicator K3.6.3."
Usage: #example
* id = "SanteonObservationExample"

* status = #final

* code.coding.system  = "http://loinc.org"
* code.coding.code    = #38445-3
* code.coding.display = "Fecal calprotectin [Mass/volume] in Stool"

* subject = Reference(ExamplePatient)

* effectiveDateTime = "2025-03-10T09:30:00Z"

* valueQuantity.value  = 250
* valueQuantity.unit   = "mg/kg"
* valueQuantity.system = "http://unitsofmeasure.org"
* valueQuantity.code   = #mg/kg

// ============================================================
// Example: Santeon Procedure (Scopie)
// IBD indicator K3.6.3
// ============================================================

Instance: SanteonProcedureExample
InstanceOf: SanteonProcedure
Title: "Voorbeeld Santeon Procedure (Scopie)"
Description: "Scopie verrichting conform het SanteonProcedure profiel. IBD use case indicator K3.6.3."
Usage: #example
* id = "SanteonProcedureExample"

* status = #completed

* code.coding[nza].system  = "urn:oid:2.16.840.1.113883.2.4.6.14"
* code.coding[nza].code    = #034620
* code.coding[nza].display = "Scopie"

* subject = Reference(ExamplePatient)

* performedPeriod.start = "2025-06-15T14:00:00Z"

// ============================================================
// Example: Santeon MeasureReport (K3.6.3 resultaat)
// Rapportage Q1 2025
// ============================================================

Instance: SanteonMeasureReportK363Example
InstanceOf: SanteonMeasureReport
Title: "Voorbeeld MeasureReport K3.6.3 (Q1 2025)"
Description: "Rapportage van IBD indicator K3.6.3 over Q1 2025. Van 120 scopieën had 78% een calprotectine meting binnen 90 dagen."
Usage: #example
* id = "SanteonMeasureReportK363Example"

* status = #complete
* type = #summary
* measure = "https://ig.santeon.nl/ibd/Measure/ibd-k363"
* date = "2025-04-15T10:00:00Z"

* period.start = "2025-01-01"
* period.end   = "2025-03-31"

* group[+].code.coding.system = "https://ig.santeon.nl/ibd/CodeSystem/santeon-indicator"
* group[=].code.coding.code = #K3.6.3
* group[=].code.coding.display = "Calprotectine vóór scopie"

* group[=].population[+].code = http://terminology.hl7.org/CodeSystem/measure-population#initial-population
* group[=].population[=].count = 95

* group[=].population[+].code = http://terminology.hl7.org/CodeSystem/measure-population#denominator
* group[=].population[=].count = 120

* group[=].population[+].code = http://terminology.hl7.org/CodeSystem/measure-population#numerator
* group[=].population[=].count = 94

* group[=].measureScore.value = 0.783
* group[=].measureScore.unit = "%"
* group[=].measureScore.system = "http://unitsofmeasure.org"
* group[=].measureScore.code = #%

// ============================================================
// Example: Individueel MeasureReport (K3.6.3 per patiënt)
// ============================================================

Instance: SanteonMeasureReportK363Individual
InstanceOf: SanteonMeasureReport
Title: "Voorbeeld MeasureReport K3.6.3 (individueel)"
Description: "Individueel MeasureReport voor één patiënt, met verwijzingen naar de geëvalueerde Observation (calprotectine) en Procedure (scopie)."
Usage: #example
* id = "SanteonMeasureReportK363Individual"

* status = #complete
* type = #individual
* measure = "https://ig.santeon.nl/ibd/Measure/ibd-k363"
* date = "2025-07-01T12:00:00Z"
* subject = Reference(ExamplePatient)

* period.start = "2025-01-01"
* period.end   = "2025-06-30"

* group[+].code.coding.system = "https://ig.santeon.nl/ibd/CodeSystem/santeon-indicator"
* group[=].code.coding.code = #K3.6.3
* group[=].code.coding.display = "Calprotectine vóór scopie"

* group[=].population[+].code = http://terminology.hl7.org/CodeSystem/measure-population#initial-population
* group[=].population[=].count = 1

* group[=].population[+].code = http://terminology.hl7.org/CodeSystem/measure-population#denominator
* group[=].population[=].count = 1

* group[=].population[+].code = http://terminology.hl7.org/CodeSystem/measure-population#numerator
* group[=].population[=].count = 1

* group[=].measureScore.value = 1.0
* group[=].measureScore.unit = "%"
* group[=].measureScore.system = "http://unitsofmeasure.org"
* group[=].measureScore.code = #%

// Link naar de concrete resources die geëvalueerd zijn
* evaluatedResource[+] = Reference(SanteonObservationExample)
* evaluatedResource[+] = Reference(SanteonProcedureExample)
