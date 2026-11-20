// =====================================================================
// sb-cva.fsh
// The sb-cva dataset (small example, based on HipsETL dataset_sb-cva.yaml):
// its cohort and the data requirements whose data it releases.
//
// Compared with sb-ibd it shows:
// - its own cohort (stroke diagnoses instead of IBD DBCs);
// - its own data requirements (NIHSS, acute CVA procedures);
// - a shared data requirement (luscii-prems), defined once in
//   DataRequirements.fsh and used by sb-ibd as well.
// Generated from this file: the Group sb-cva-cohort, the Parameters
// sb-cva-export, the profiles SbCvaPatient, SbCvaObservation,
// SbCvaProcedure and SbCvaQuestionnaireResponse, and the page sb-cva.html.
// =====================================================================

Instance: sb-cva
InstanceOf: DatasetDefinition
Usage: #definition
Title: "sb-cva"
Description: "Dataset for adult stroke (CVA) patients."
* id = "sb-cva"
* version = "0.1.0"
* title = "sb-cva"
* description = "Gegevensset voor volwassen patiënten met een herseninfarct of hersenbloeding (CVA): NIHSS, acute verrichtingen en de Luscii PREMs."
* cohort.description = """
Alle patiënten van 18 jaar of ouder op het moment van aanleveren met een diagnose herseninfarct (ICD-10 I63)
of hersenbloeding (ICD-10 I61, I62), inclusief subcodes, met een begindatum op of na 01-01-2018.
In HipsETL komt het cohort daarnaast uit DBC's neurologie/cardiologie en uit de DASA-registratie; dat is
in dit voorbeeld weggelaten.
"""
* cohort.criteria[+].resource = #Patient
* cohort.criteria[=].param[+].name = "birthdate"
* cohort.criteria[=].param[=].value = "le{today-18y}"
* cohort.criteria[+].resource = #Condition
* cohort.criteria[=].param[+].name = "code:below"
* cohort.criteria[=].param[=].value = "http://hl7.org/fhir/sid/icd-10-nl|I61,http://hl7.org/fhir/sid/icd-10-nl|I62,http://hl7.org/fhir/sid/icd-10-nl|I63"
* cohort.criteria[=].param[+].name = "onset-date"
* cohort.criteria[=].param[=].value = "ge2018-01-01"

* include[+].resource = #Patient
* include[=].param[+].name = "gender"
* include[=].param[=].value = "male,female"
* include[=].element[+] = "gender"
* include[=].element[+] = "birthDate"

* requirement[+] = "cva-nihss"
* requirement[+] = "cva-acute-procedures"
* requirement[+] = "luscii-prems"
