// =====================================================================
// DataRequirements.fsh
// Data requirements: which data, with which FHIR search filters and which
// elements, for a KPI, a questionnaire or any other use. No roles
// (numerator/denominator) — that is decided downstream.
// =====================================================================

// ---------------------------------------------------------------------
// Group requirements: who is in the Group of a dataset. A dataset refers
// to one with `* group = "<id>"`; each criterion becomes a member-filter.
// ---------------------------------------------------------------------

Instance: req-sb-ibd-group
InstanceOf: DataRequirementDefinition
Usage: #definition
Title: "IBD bij volwassenen (Group sb-ibd)"
Description: "Patients in the sb-ibd Group: adults with an IBD DBC since 2018."
* id = "sb-ibd-group"
* version = "0.1.0"
* title = "IBD bij volwassenen (Group sb-ibd)"
* description = """
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
* criteria[+].resource = #Patient
* criteria[=].param[+].name = "birthdate"
* criteria[=].param[=].value = "le{today-18y}"
* criteria[+].resource = #EpisodeOfCare
// Zorgtype als losse codes: 11 regulier DBC-zorgtraject, 21 regulier vervolg-DBC-zorgtraject.
* criteria[=].param[+].name = "type"
* criteria[=].param[=].value = "https://declaratie.nza.nl/zorgtype|11,https://declaratie.nza.nl/zorgtype|21"
* criteria[=].param[+].name = "status"
* criteria[=].param[=].value = "active,finished"
* criteria[=].param[+].name = "date"
* criteria[=].param[=].value = "ge2018-01-01"
// DBC-diagnosecodes (DBC Onderhoud): één codesysteem per specialisme,
// urn:oid:2.16.840.1.113883.2.4.3.27.15.4.<specialisme zonder voorloopnullen>.
// 0313 Interne Geneeskunde 922, 923 (internist MDL) en 0318 Maag-Darm-Leverziekten 601, 602.
// R4-zoekparameter condition (EpisodeOfCare.diagnosis.condition), geketend naar Condition.code.
* criteria[=].param[+].name = "condition:Condition.code"
* criteria[=].param[=].value = "urn:oid:2.16.840.1.113883.2.4.3.27.15.4.313|922,urn:oid:2.16.840.1.113883.2.4.3.27.15.4.313|923,urn:oid:2.16.840.1.113883.2.4.3.27.15.4.318|601,urn:oid:2.16.840.1.113883.2.4.3.27.15.4.318|602"
* postFilter[+].description = "De DOT moet op of na 01-01-2018 geopend zijn. Een FHIR-datumzoekactie op EpisodeOfCare.period (`date=ge2018-01-01`) selecteert elke DOT die 2018 of later overlapt, dus ook een DOT die eerder begon; de begindatum wordt daarom na de query gecontroleerd."
* postFilter[=].expression = "EpisodeOfCare.period.start >= @2018-01-01"

Instance: req-sb-cva-group
InstanceOf: DataRequirementDefinition
Usage: #definition
Title: "CVA bij volwassenen (Group sb-cva)"
Description: "Patients in the sb-cva Group: adults with a stroke diagnosis since 2018."
* id = "sb-cva-group"
* version = "0.1.0"
* title = "CVA bij volwassenen (Group sb-cva)"
* description = """
Alle patiënten van 18 jaar of ouder op het moment van aanleveren met een diagnose herseninfarct (ICD-10 I63)
of hersenbloeding (ICD-10 I61, I62), inclusief subcodes, met een begindatum op of na 01-01-2018.
In HipsETL komen de patiënten daarnaast ook uit DBC's neurologie/cardiologie en uit de DASA-registratie; dat is
in dit voorbeeld weggelaten.
"""
* criteria[+].resource = #Patient
* criteria[=].param[+].name = "birthdate"
* criteria[=].param[=].value = "le{today-18y}"
* criteria[+].resource = #Condition
* criteria[=].param[+].name = "code:below"
* criteria[=].param[=].value = "http://hl7.org/fhir/sid/icd-10-nl|I61,http://hl7.org/fhir/sid/icd-10-nl|I62,http://hl7.org/fhir/sid/icd-10-nl|I63"
* criteria[=].param[+].name = "onset-date"
* criteria[=].param[=].value = "ge2018-01-01"

// ---------------------------------------------------------------------
// Data requirements of KPIs and other uses
// ---------------------------------------------------------------------

Instance: req-k312-scopie
InstanceOf: DataRequirementDefinition
Usage: #definition
Title: "K3.1.2 Scopie"
Description: "Number of adult IBD patients with a scopie since 2018."
* id = "k312-scopie"
* version = "0.1.0"
* title = "K3.1.2 Scopie"
* description = "Aantal volwassen IBD-patiënten met een scopie-verrichting vanaf 2018. Alleen de NZa-scopiecodes 034620, 034686, 034690 en 035582; de codes staan inline."
* criteria[+].resource = #Procedure
* criteria[=].param[+].name = "code"
* criteria[=].param[=].value = "urn:oid:2.16.840.1.113883.2.4.3.27.15.5|034620,urn:oid:2.16.840.1.113883.2.4.3.27.15.5|034686,urn:oid:2.16.840.1.113883.2.4.3.27.15.5|034690,urn:oid:2.16.840.1.113883.2.4.3.27.15.5|035582"
* criteria[=].param[+].name = "date"
* criteria[=].param[=].value = "ge2018-01-01"
* criteria[=].element[+] = "code"
* criteria[=].element[+] = "performed[x]"
* criteria[=].element[+] = "subject"

Instance: req-k363-calprotectin-before-scopie
InstanceOf: DataRequirementDefinition
Usage: #definition
Title: "K3.6.3 Calprotectine voor scopie"
Description: "Share of scopies preceded by a faecal calprotectin measurement."
* id = "k363-calprotectin-before-scopie"
* version = "0.1.0"
* title = "K3.6.3 Calprotectine voor scopie"
* description = "Percentage scopieën waarbij in de 90 dagen voorafgaand aan de scopie ten minste één fecaal calprotectinemeting is uitgevoerd."
* criteria[+].resource = #Procedure
* criteria[=].param[+].name = "code:in"
* criteria[=].param[=].value = "{vs:ibd-verrichting-codes-nza}"
* criteria[=].param[+].name = "date"
* criteria[=].param[=].value = "ge2018-01-01"
* criteria[=].element[+] = "code"
* criteria[=].element[+] = "performed[x]"
* criteria[=].element[+] = "subject"
* criteria[+].resource = #Observation
* criteria[=].param[+].name = "code"  
* criteria[=].param[=].value = "{od:od-calprotectin.code}"  // Reference to the observation definition.
* criteria[=].param[+].name = "date"
* criteria[=].param[=].value = "ge2018-01-01"
* criteria[=].element[+] = "code"
* criteria[=].element[+] = "value[x]"
* criteria[=].element[+] = "effective[x]"
* criteria[=].element[+] = "subject"
* postFilter[+].description = "A calprotectin measurement counts only when it falls within the 90 days up to and including the start of a scopie of the same patient. A date relation between two resources cannot be expressed in a FHIR search."
* postFilter[=].expression = "Observation.effective >= Procedure.performed.start - 90 days and Observation.effective <= Procedure.performed.start and Observation.subject = Procedure.subject"

Instance: req-alcohol-status-recorded
InstanceOf: DataRequirementDefinition
Usage: #definition
Title: "Alcoholgebruik vastgelegd"
Description: "Alcohol use status recorded, with a coded answer."
* id = "alcohol-status-recorded"
* version = "0.1.0"
* title = "Alcoholgebruik vastgelegd"
* description = "Is de alcoholgebruikstatus vastgelegd met een gecodeerde uitslag?"
* criteria[+].resource = #Observation
* criteria[=].param[+].name = "code"
* criteria[=].param[=].value = "{od:od-alcohol-use.code}"  // Reference to the observation definition.
* criteria[=].param[+].name = "date" 
* criteria[=].param[=].value = "ge2018-01-01"
* criteria[=].element[+] = "code"
* criteria[=].element[+] = "value[x]"
* criteria[=].element[+] = "effective[x]"
* criteria[=].element[+] = "subject"

Instance: req-cva-nihss
InstanceOf: DataRequirementDefinition
Usage: #definition
Title: "NIHSS bij CVA"
Description: "NIH Stroke Scale total score, for the severity of a stroke."
* id = "cva-nihss"
* version = "0.1.0"
* title = "NIHSS bij CVA"
* description = "NIHSS-totaalscore (ernst van het CVA) vanaf 2018. Bron: HipsETL dataset_sb-cva.yaml, AlgemeneMeting (LOINC 72089-6)."
* criteria[+].resource = #Observation
* criteria[=].param[+].name = "code"
* criteria[=].param[=].value = "{od:od-nihss-total.code}"  // Reference to the observation definition.
* criteria[=].param[+].name = "date"
* criteria[=].param[=].value = "ge2018-01-01"
* criteria[=].element[+] = "code"
* criteria[=].element[+] = "value[x]"
* criteria[=].element[+] = "effective[x]"
* criteria[=].element[+] = "subject"

Instance: req-cva-acute-procedures
InstanceOf: DataRequirementDefinition
Usage: #definition
Title: "Acute CVA-verrichtingen"
Description: "Stroke procedures coded in SNOMED CT: imaging, thrombolysis, thrombectomy, bladder catheter."
* id = "cva-acute-procedures"
* version = "0.1.0"
* title = "Acute CVA-verrichtingen"
* description = "Inbrengen blaaskatheter, blaaskatheter, CTA van hoofd, CT perfusie van hersenen, CTA van aorta en carotis, MRA van hoofd, IVT, IAT. Bron: HipsETL dataset_sb-cva.yaml, Verrichting (VerrichtingTypeCodeSnomedCT). De codes staan inline; ze komen in de gegenereerde code value set van sb-cva."
* criteria[+].resource = #Procedure
* criteria[=].param[+].name = "code"
* criteria[=].param[=].value = "http://snomed.info/sct|410024004,http://snomed.info/sct|582101000119108,http://snomed.info/sct|433111008,http://snomed.info/sct|417986000,http://snomed.info/sct|417936006,http://snomed.info/sct|77381000146102,http://snomed.info/sct|397046001,http://snomed.info/sct|43748006"
* criteria[=].param[+].name = "date"
* criteria[=].param[=].value = "ge2018-01-01"
* criteria[=].element[+] = "code"
* criteria[=].element[+] = "performed[x]"
* criteria[=].element[+] = "subject"

Instance: req-luscii-prems
InstanceOf: DataRequirementDefinition
Usage: #definition
Title: "Luscii PREMs"
Description: "Completed Luscii PREM questionnaires (remote care experience)."
* id = "luscii-prems"
* version = "0.1.0"
* title = "Luscii PREMs"
* description = "Ingevulde Luscii PREM-vragenlijsten (ervaring met zorg op afstand), vragen IC_000253 t/m IC_000257. De vragenlijst komt uit ART-DECOR; het hele formulier wordt opgevraagd."
* criteria[+].resource = #QuestionnaireResponse
* criteria[=].param[+].name = "questionnaire"
* criteria[=].param[=].value = "{q:luscii-prems}"  // Reference to the questionnaire (external-definitions.yaml).
* criteria[=].param[+].name = "authored"
* criteria[=].param[=].value = "ge2018-01-01"
* criteria[=].element[+] = "questionnaire"
* criteria[=].element[+] = "authored"
* criteria[=].element[+] = "item"
* criteria[=].element[+] = "subject"
