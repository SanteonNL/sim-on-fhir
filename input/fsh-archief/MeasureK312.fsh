Instance: MeasureK312
InstanceOf: Measure
Title: "Indicator K3.1.2 - % patiënten dat een scopie krijgt"
Description: "Kwaliteitsindicator voor het percentage IBD-patiënten dat een scopie ondergaat."

* url = "https://santeon.nl"
* version = "1.0.0"
* status = #active
* scoring = #proportion
* library = "https://santeon.nl"

// Cohort / Initial Population
* group[0].population[0].code = #initial-population
* group[0].population[0].criteria.language = #text/cql
* group[0].population[0].criteria.expression = "IBDCohort"

// Denominator (Noemer)
* group[0].population[1].code = #denominator
* group[0].population[1].criteria.language = #text/cql
* group[0].population[1].criteria.expression = "IBDCohort"

// Numerator (Teller)
* group[0].population[2].code = #numerator
* group[0].population[2].criteria.language = #text/cql
* group[0].population[2].criteria.expression = "Numerator"
