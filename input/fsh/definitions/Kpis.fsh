// =====================================================================
// Kpis.fsh
// KPI definitions: which data, with which FHIR search filters. No roles
// (numerator/denominator) — that is decided downstream.
// =====================================================================

Instance: kpi-k312-scopie
InstanceOf: KpiDefinition
Usage: #definition
Title: "K3.1.2 Scopie"
Description: "Number of adult IBD patients with a scopie since 2018."
* id = "k312-scopie"
* version = "0.1.0"
* title = "K3.1.2 Scopie"
* description = "Aantal volwassen IBD-patiënten met een scopie-verrichting vanaf 2018."
* criteria[+].resource = #Procedure
* criteria[=].param[+].name = "code:in"
* criteria[=].param[=].value = "{vs:local-verrichting-codes-nza}"
* criteria[=].param[+].name = "date"
* criteria[=].param[=].value = "ge2018-01-01"
* criteria[=].element[+] = "code"
* criteria[=].element[+] = "performed[x]"
* criteria[=].element[+] = "subject"

Instance: kpi-k363-calprotectin-before-scopie
InstanceOf: KpiDefinition
Usage: #definition
Title: "K3.6.3 Calprotectine voor scopie"
Description: "Share of scopies preceded by a faecal calprotectin measurement."
* id = "k363-calprotectin-before-scopie"
* version = "0.1.0"
* title = "K3.6.3 Calprotectine voor scopie"
* description = "Percentage scopieën waarbij in de 90 dagen voorafgaand aan de scopie ten minste één fecaal calprotectinemeting is uitgevoerd."
* criteria[+].resource = #Procedure
* criteria[=].param[+].name = "code:in"
* criteria[=].param[=].value = "{vs:local-verrichting-codes-nza}"
* criteria[=].param[+].name = "date"
* criteria[=].param[=].value = "ge2018-01-01"
* criteria[=].element[+] = "code"
* criteria[=].element[+] = "performed[x]"
* criteria[=].element[+] = "subject"
* criteria[+].resource = #Observation
* criteria[=].param[+].name = "code"
* criteria[=].param[=].value = "{od:od-calprotectin.code}"
* criteria[=].param[+].name = "date"
* criteria[=].param[=].value = "ge2018-01-01"
* criteria[=].element[+] = "code"
* criteria[=].element[+] = "value[x]"
* criteria[=].element[+] = "effective[x]"
* criteria[=].element[+] = "subject"
* postFilter[+].description = "A calprotectin measurement counts only when it falls within the 90 days up to and including the start of a scopie of the same patient. A date relation between two resources cannot be expressed in a FHIR search."
* postFilter[=].expression = "Observation.effective >= Procedure.performed.start - 90 days and Observation.effective <= Procedure.performed.start and Observation.subject = Procedure.subject"

Instance: kpi-alcohol-status-recorded
InstanceOf: KpiDefinition
Usage: #definition
Title: "Alcoholgebruik vastgelegd"
Description: "Alcohol use status recorded, with a coded answer."
* id = "alcohol-status-recorded"
* version = "0.1.0"
* title = "Alcoholgebruik vastgelegd"
* description = "Is de alcoholgebruikstatus vastgelegd met een gecodeerde uitslag?"
* criteria[+].resource = #Observation
* criteria[=].param[+].name = "code"
* criteria[=].param[=].value = "{od:od-alcohol-use.code}"
* criteria[=].param[+].name = "date"
* criteria[=].param[=].value = "ge2018-01-01"
* criteria[=].element[+] = "code"
* criteria[=].element[+] = "value[x]"
* criteria[=].element[+] = "effective[x]"
* criteria[=].element[+] = "subject"
