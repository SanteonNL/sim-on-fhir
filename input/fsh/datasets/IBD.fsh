// =====================================================================
// IBD.fsh
// The IBD dataset: its cohort and the KPI definitions whose data it releases.
// One file per dataset in this folder; each dataset becomes one export on
// one Group. Generated from this file: the Group <id>-cohort, the Parameters
// <id>-export and the page <id>.html.
// =====================================================================

Instance: ibd-dataset
InstanceOf: DatasetDefinition
Usage: #definition
Title: "IBD dataset"
Description: "Dataset for adult IBD patients."
* id = "ibd-dataset"
* version = "0.1.0"
* title = "IBD"
* description = "Gegevensset voor volwassen patiënten met inflammatoire darmziekten (IBD): scopieën en calprotectinemetingen, plus alcoholgebruik."
* cohort.description = """
Alle patiënten van 18 jaar of ouder op het moment van aanleveren met een actief, regulier DBC-zorgtraject
(zorgtype 11 of 21) dat op of na 01-01-2018 is geopend en waarvan de specialisme-diagnose bij IBD hoort.
De startdatum 01-01-2018 is bewust niet dynamisch.
"""
* cohort.criteria[+].resource = #Patient
* cohort.criteria[=].param[+].name = "birthdate"
* cohort.criteria[=].param[=].value = "le{today-18y}"
* cohort.criteria[+].resource = #EpisodeOfCare
* cohort.criteria[=].param[+].name = "type:in"
* cohort.criteria[=].param[=].value = "{vs:local-zorgtype-codes}"
* cohort.criteria[=].param[+].name = "status"
* cohort.criteria[=].param[=].value = "active"
* cohort.criteria[=].param[+].name = "date"
* cohort.criteria[=].param[=].value = "ge2018-01-01"
* cohort.criteria[=].param[+].name = "diagnosis:Condition.code:in"
* cohort.criteria[=].param[=].value = "{vs:local-specialisme-diagnose-codes}"

* include[+].resource = #Patient
* include[=].param[+].name = "gender"
* include[=].param[=].value = "male,female"
* include[=].element[+] = "gender"
* include[=].element[+] = "birthDate"

* kpi[+] = "k312-scopie"
* kpi[+] = "k363-calprotectin-before-scopie"
* kpi[+] = "alcohol-status-recorded"
