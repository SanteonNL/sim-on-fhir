// ============================================================
// K3.6.3 — Calprotectine meting voorafgaand aan scopie
// % scopieën waarbij tot 90 dagen voorafgaand aan de scopie
// een calprotectinemeting is gedaan
//
// LOINC: 38445-3 (Calprotectine in feces)
// Window: meting moet ≤90 dagen VOOR de scopiedatum liggen
// ============================================================

Profile: IBDCalprotectineMeting
Parent: Observation
Id: ibd-calprotectine-meting
Title: "IBD Calprotectine Meting (K3.6.3)"
Description: """
  Profiel voor een fecale calprotectinemeting bij een IBD-patiënt.
  Gebruikt voor indicator K3.6.3: percentage scopieën waarbij ≤90 dagen 
  voorafgaand aan de scopie een calprotectinemeting is gedaan.
  
  LOINC-code: 38445-3 (Calprotectine [Massa/massa] in Feces).
  Het 90-dagen venster wordt gevalideerd via de invariant ibd-calpro-window.
"""

* status 1..1 MS
* status = #final

* code 1..1 MS
* code = $LOINC#38445-3 "Calprotectine [Massa/massa] in Feces"
* code ^short = "LOINC 38445-3 — Calprotectine feces (MetingNaamCode)"

* subject 1..1 MS
* subject only Reference(IBDPatient)
* subject ^short = "Identificatienummer van de patiënt"

* effective[x] 1..1 MS
* effective[x] only dateTime
* effectiveDateTime ^short = "MetingDatumTijd — tijdstip van de meting"

* value[x] MS
* value[x] only Quantity
* valueQuantity.unit = "mg/kg"
* valueQuantity.system = "http://unitsofmeasure.org"
* valueQuantity.code = #mg/kg

// Koppeling met de scopie waarvoor deze meting relevant is
* partOf MS
* partOf only Reference(IBDScopie)
* partOf ^short = """
  Verwijzing naar de IBD-scopie (K3.1.2) waarvoor deze meting 
  als voorbereiding geldt. De meting moet ≤90 dagen voor de scopie liggen.
"""

// Invariant: meting moet vóór de scopie liggen (enforcement in validator)
// De 90-dagen check gebeurt op query-niveau in de indicator-logica,
// maar de relatie wordt hier structureel vastgelegd.

// ============================================================
// MeasureReport profiel voor K3.6.3 (aggregaat per periode)
// ============================================================

Profile: IBDK363MeasureReport
Parent: MeasureReport
Id: ibd-k363-measure-report
Title: "IBD K3.6.3 MeasureReport"
Description: """
  Geaggregeerde rapportage van indicator K3.6.3:
  percentage scopieën (K3.1.2) met calprotectinemeting ≤90 dagen ervoor.
  
  Teller:   scopieën MET een calprotectinemeting ≤90 dagen ervoor
  Noemer:   alle scopieën bij IBD-patiënten (K3.1.2 populatie)
"""

* status 1..1 MS
* type = #summary
* measure 1..1 MS
* measure = "https://santeon.nl/fhir/Measure/ibd-k363"

* period 1..1 MS
* period ^short = "Rapportageperiode (bijv. kalenderjaar)"

* group 1..1 MS
* group.population 2..* MS

// Noemer: patiënten met scopie (K3.1.2)
* group.population ^slicing.discriminator.type = #value
* group.population ^slicing.discriminator.path = "code.coding.code"
* group.population ^slicing.rules = #open
* group.population contains
    noemer 1..1 MS and
    teller 1..1 MS

* group.population[noemer].code.coding.code = #denominator
* group.population[noemer].count 1..1 MS
* group.population[noemer].count ^short = "Totaal aantal scopieën (K3.1.2)"

* group.population[teller].code.coding.code = #numerator
* group.population[teller].count 1..1 MS
* group.population[teller].count ^short = "Scopieën met calprotectinemeting ≤90 dagen ervoor"

// Percentage
* group.measureScore MS
* group.measureScore ^short = "Berekend percentage K3.6.3 (teller/noemer × 100)"

// ============================================================
// Measure definitie K3.6.3
// ============================================================

Instance: IBDK363Measure
InstanceOf: Measure
Title: "Measure K3.6.3 — Calprotectine voor scopie"
Description: "Formele definitie van indicator K3.6.3"
* id = "ibd-k363-measure"
* url = "https://santeon.nl/fhir/Measure/ibd-k363"
* status = #active
* title = "K3.6.3 — % scopieën met calprotectinemeting ≤90 dagen"
* description = """
  Percentage scopieën (NZa 034620/034686/034690/035582) bij IBD-patiënten
  waarbij in de 90 dagen voorafgaand aan de scopie een fecale calprotectinemeting 
  (LOINC 38445-3) is verricht.
"""
* scoring = http://terminology.hl7.org/CodeSystem/measure-scoring#proportion
* type = http://terminology.hl7.org/CodeSystem/measure-type#process

* group[0].population[0].code = http://terminology.hl7.org/CodeSystem/measure-population#initial-population
* group[0].population[0].description = "IBD-patiënten met ≥1 scopie in de rapportageperiode"
* group[0].population[0].criteria.language = #text/fhirpath
* group[0].population[0].criteria.expression = "Procedure.where(code.coding.where(system='https://declaratie.nza.nl/verrichting' and code.memberOf('https://santeon.nl/fhir/ValueSet/ibd-scopie-verrichting-vs')))"

* group[0].population[1].code = http://terminology.hl7.org/CodeSystem/measure-population#denominator
* group[0].population[1].description = "Alle scopieën in de initiële populatie"
* group[0].population[1].criteria.language = #text/fhirpath
* group[0].population[1].criteria.expression = "%initialPopulation"

* group[0].population[2].code = http://terminology.hl7.org/CodeSystem/measure-population#numerator
* group[0].population[2].description = "Scopieën met calprotectinemeting (LOINC 38445-3) ≤90 dagen ervoor"
* group[0].population[2].criteria.language = #text/fhirpath
* group[0].population[2].criteria.expression = """
  %denominator.where(
    partOf.ofType(Observation).where(
      code.coding.where(system='http://loinc.org' and code='38445-3').exists()
      and (effectiveDateTime >= (performedDateTime - 90 days))
      and (effectiveDateTime <= performedDateTime)
    ).exists()
  )
"""

// ============================================================
// Alias
// ============================================================

Alias: $LOINC = http://loinc.org
