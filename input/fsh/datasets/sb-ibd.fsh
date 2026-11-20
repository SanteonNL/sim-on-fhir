// =====================================================================
// sb-ibd.fsh
// The sb-ibd dataset: its cohort and the data requirements whose data it
// releases. One file per dataset in this folder; each dataset becomes one
// export on one Group. Generated from this file: the Group <id>-cohort, the
// Parameters <id>-export, one profile per resource type (<Id><Resource>,
// derived from the SAN profile) and the page <id>.html.
// =====================================================================

Instance: sb-ibd
InstanceOf: DatasetDefinition
Usage: #definition
Title: "sb-ibd"
Description: "Dataset for adult IBD patients."
* id = "sb-ibd"
* version = "0.1.0"
* title = "sb-ibd"
* description = "Gegevensset voor volwassen patiënten met inflammatoire darmziekten (IBD): verrichtingen (waaronder scopieën), calprotectinemetingen en de Luscii PREMs."
* cohort.description = """
**IBD bij volwassenen** (HIX: cohort_sb-ibd_dbc.sql). Alle patiënten van 18 jaar of ouder op het moment van
aanleveren met een geldige DOT (niet vervallen) met zorgtype 11 of 21, geopend op of na 01-01-2018, en:

- 0313 Interne Geneeskunde met diagnose 922 of 923 (internist met specialisatie maag-darm-leverziekten); of
- 0318 Maag-Darm-Leverziekten met diagnose 601 of 602.

De startdatum 01-01-2018 is bewust niet dynamisch.

**Nog niet in deze definitie** (staat wel in de cohortbeschrijving van de query):

- *IBD bij kinderen*: een Paris-classificatie (Crohn of CU) op of na 01-01-2020 én een DOT 0316
  Kindergeneeskunde met diagnose 3303, 3308, 3311, 3314, 3315 of 3399, zonder leeftijdsgrens. Dit is een
  tweede, alternatieve tak (OF); een Group met member-filters kent alleen EN.
- *ZBJ_IBD*: patiënten met een CarePlan-categorie ZBJ_IBD gestart op of na 01-01-2018 (alleen in de
  toelichting van de query, niet in de SQL zelf).
"""
* cohort.criteria[+].resource = #Patient
* cohort.criteria[=].param[+].name = "birthdate"
* cohort.criteria[=].param[=].value = "le{today-18y}"
* cohort.criteria[+].resource = #EpisodeOfCare
// Zorgtype als losse codes: 11 regulier DBC-zorgtraject, 21 regulier vervolg-DBC-zorgtraject.
* cohort.criteria[=].param[+].name = "type"
* cohort.criteria[=].param[=].value = "https://declaratie.nza.nl/zorgtype|11,https://declaratie.nza.nl/zorgtype|21"
* cohort.criteria[=].param[+].name = "status"
* cohort.criteria[=].param[=].value = "active,finished"
* cohort.criteria[=].param[+].name = "date"
* cohort.criteria[=].param[=].value = "ge2018-01-01"
// Specialisme-diagnose als losse codes: 0313 Interne Geneeskunde 922, 923 (internist MDL)
// en 0318 Maag-Darm-Leverziekten 601, 602. De code bevat zelf een '|'; in een FHIR-zoekwaarde
// wordt die geschreven als '\|' (de eerste, onge-escapete '|' scheidt systeem en code).
* cohort.criteria[=].param[+].name = "diagnosis:Condition.code"
* cohort.criteria[=].param[=].value = "https://ig.santeon.nl/CodeSystem/specialisme-diagnose|0313\\|922,https://ig.santeon.nl/CodeSystem/specialisme-diagnose|0313\\|923,https://ig.santeon.nl/CodeSystem/specialisme-diagnose|0318\\|601,https://ig.santeon.nl/CodeSystem/specialisme-diagnose|0318\\|602"
* cohort.postFilter[+].description = "De DOT moet op of na 01-01-2018 geopend zijn. Een FHIR-datumzoekactie op EpisodeOfCare.period (`date=ge2018-01-01`) selecteert elke DOT die 2018 of later overlapt, dus ook een DOT die eerder begon; de begindatum wordt daarom na de query gecontroleerd."
* cohort.postFilter[=].expression = "EpisodeOfCare.period.start >= @2018-01-01"

* include[+].resource = #Patient
* include[=].param[+].name = "gender"
* include[=].param[=].value = "male,female"
* include[=].element[+] = "gender"
* include[=].element[+] = "birthDate"

* requirement[+] = "k312-scopie"
* requirement[+] = "k363-calprotectin-before-scopie"
* requirement[+] = "luscii-prems"
