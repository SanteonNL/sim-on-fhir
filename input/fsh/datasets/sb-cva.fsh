// =====================================================================
// sb-cva.fsh
// The sb-cva dataset (small example, based on HipsETL dataset_sb-cva.yaml):
// a reference to its Group requirement (in DataRequirements.fsh) and the data requirements whose data it releases.
//
// Compared with sb-ibd it shows:
// - its own Group requirement (stroke diagnoses instead of IBD DBCs);
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
* group = "sb-cva-group"

* include[+].resource = #Patient
* include[=].param[+].name = "gender"
* include[=].param[=].value = "male,female"
* include[=].element[+] = "gender"
* include[=].element[+] = "birthDate"

* requirement[+] = "cva-nihss"
* requirement[+] = "cva-acute-procedures"
* requirement[+] = "luscii-prems"
