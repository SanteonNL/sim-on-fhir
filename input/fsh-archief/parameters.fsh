Alias: $NZa        = https://declaratie.nza.nl/zorgactiviteit
Alias: $DotSpec    = https://declaratie.nza.nl/specialisme
Alias: $DotDiag    = https://declaratie.nza.nl/diagnose
Alias: $Zorgtype   = https://declaratie.nza.nl/zorgtype
Alias: $CarePlanCS = https://jouworganisatie.nl/fhir/CodeSystem/careplan-category

// Scopie codes (indicator numerator)
ValueSet: VsScopieNza
Id: vs-scopie-nza
Title: "NZa Scopie Codes K312"
* $NZa#034620 "Diagnostische endoscopie oesofagus/maag/duodenum"
* $NZa#034686 "Diagnostische coloscopie"
* $NZa#034690 "Diagnostische sigmoidoscopie"
* $NZa#035582 "Coloscopie variant"

// Zorgtype codes (cohort DBC)
ValueSet: VsZorgtypeCohort
Id: vs-zorgtype-cohort
Title: "Zorgtype codes IBD cohort"
* $Zorgtype#11
* $Zorgtype#21

// IBD diagnosecodes MDL (0318)
ValueSet: VsIbdDiagnoseMdl
Id: vs-ibd-diagnose-mdl
Title: "IBD Diagnosecodes MDL specialisme 0318"
* $DotDiag#601
* $DotDiag#602

// IBD diagnosecodes Interne (0313)
ValueSet: VsIbdDiagnoseInterne
Id: vs-ibd-diagnose-interne
Title: "IBD Diagnosecodes Interne Geneeskunde specialisme 0313"
* $DotDiag#922
* $DotDiag#923