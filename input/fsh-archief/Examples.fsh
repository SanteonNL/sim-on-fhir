






/* mag wat mij betreft allemaal weg/herschreven worden:

// ============================================================
// Voorbeeldinstanties — IBD IG
// Gebruikt voor documentatie én validatietests
// ============================================================

// --- Patiënt ------------------------------------------------

Instance: VoorbeeldIBDPatient
InstanceOf: IBDPatient
Title: "Voorbeeld IBD Patiënt"
Description: "Testpatiënt voor IBD-cohort, geboren 1975 (>18 jaar)"
* identifier[patientnummer].system = "https://santeon.nl/fhir/NamingSystem/patientnummer"
* identifier[patientnummer].value = "P123456"
* birthDate = "1975-06-15"
* gender = #male

// --- EpisodeOfCare (DOT/Subtraject) -------------------------

Instance: VoorbeeldIBDEpisode
InstanceOf: IBDEpisodeOfCare
Title: "Voorbeeld IBD Subtraject (DOT)"
Description: "DOT specialisme 0318, diagnosecode 601, zorgtype 11"
* status = #active
* type = http://snomed.info/sct#24028007 "IBD zorgtraject"
* patient = Reference(VoorbeeldIBDPatient)
* period.start = "2024-03-01"
* extension[specialisme].valueCoding = https://santeon.nl/fhir/CodeSystem/nza-specialisme#0318 "MDL"
* extension[diagnoseCode].valueCoding = https://santeon.nl/fhir/CodeSystem/dbc-diagnose#601 "Colitis ulcerosa"
* extension[zorgtype].valueCoding = https://santeon.nl/fhir/CodeSystem/nza-zorgtype#11 "Regulier"

// --- Condition (Diagnose) -----------------------------------

Instance: VoorbeeldIBDDiagnose
InstanceOf: IBDCondition
Title: "Voorbeeld IBD Diagnose"
Description: "Colitis ulcerosa (diagnosecode 601)"
* clinicalStatus = http://terminology.hl7.org/CodeSystem/condition-clinical#active
* code = https://santeon.nl/fhir/CodeSystem/dbc-diagnose#601 "Colitis ulcerosa"
* subject = Reference(VoorbeeldIBDPatient)

// --- Contact (Encounter) ------------------------------------

Instance: VoorbeeldIBDContact
InstanceOf: IBDContact
Title: "Voorbeeld Poliklinisch Contact"
Description: "Poliklinisch bezoek (SS) voor IBD-scopie"
* status = #finished
* class = http://terminology.hl7.org/CodeSystem/v3-ActCode#AMB "SS — Poliklinisch"
* subject = Reference(VoorbeeldIBDPatient)
* period.start = "2024-06-10T09:00:00Z"
* period.end  = "2024-06-10T10:30:00Z"
* episodeOfCare = Reference(VoorbeeldIBDEpisode)

// --- Scopie (K3.1.2) ----------------------------------------

Instance: VoorbeeldScopie
InstanceOf: IBDScopie
Title: "Voorbeeld Colonoscopie (K3.1.2)"
Description: "Colonoscopie NZa-code 034620, uitgevoerd 10 juni 2024"
* status = #completed
* code = https://declaratie.nza.nl/verrichting#034620 "Colonoscopie diagnostisch"
* subject = Reference(VoorbeeldIBDPatient)
* performedDateTime = "2024-06-10T09:30:00Z"
* performer.actor = Reference(VoorbeeldUitvoerder)
* encounter = Reference(VoorbeeldIBDContact)
* reasonReference = Reference(VoorbeeldIBDDiagnose)

// --- Calprotectinemeting (K3.6.3) ---------------------------
// Meting 45 dagen vóór de scopie → telt mee voor K3.6.3 ✓

Instance: VoorbeeldCalprotectine
InstanceOf: IBDCalprotectineMeting
Title: "Voorbeeld Calprotectine Meting (K3.6.3)"
Description: "Calprotectinemeting 45 dagen voor scopie (LOINC 38445-3) — valt binnen 90-dagen venster"
* status = #final
* code = http://loinc.org#38445-3 "Calprotectine [Massa/massa] in Feces"
* subject = Reference(VoorbeeldIBDPatient)
* effectiveDateTime = "2024-04-26T08:00:00Z"   // 45 dagen vóór 2024-06-10
* valueQuantity.value = 187
* valueQuantity.unit = "mg/kg"
* valueQuantity.system = "http://unitsofmeasure.org"
* valueQuantity.code = #mg/kg
* partOf = Reference(VoorbeeldScopie)

// --- Uitvoerder (PractitionerRole) --------------------------

Instance: VoorbeeldUitvoerder
InstanceOf: PractitionerRole
Title: "Voorbeeld Uitvoerder MDL-arts"
Description: "MDL-arts met specialisme 0318, uitvoerder van de scopie"
* active = true
* specialty = https://santeon.nl/fhir/CodeSystem/nza-specialisme#0318 "MDL"






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

*/
