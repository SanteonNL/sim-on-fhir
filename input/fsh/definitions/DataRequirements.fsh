// =====================================================================
// DataRequirements.fsh
// Data requirements: which data, with which FHIR search filters and which
// elements, for a KPI, a questionnaire or any other use. No roles
// (numerator/denominator) — that is decided downstream.
// =====================================================================

Instance: req-k312-scopie
InstanceOf: DataRequirementDefinition
Usage: #definition
Title: "K3.1.2 Scopie"
Description: "Number of adult IBD patients with a scopie since 2018."
* id = "k312-scopie"
* version = "0.1.0"
* title = "K3.1.2 Scopie"
* description = "Aantal volwassen IBD-patiënten met een scopie-verrichting vanaf 2018."
* criteria[+].resource = #Procedure
* criteria[=].param[+].name = "code:in"
* criteria[=].param[=].value = "{vs:ibd-verrichting-codes-nza}"
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
