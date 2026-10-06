// =====================================================================
// ObservationDefinitions.fsh
// One ObservationDefinition per measurement. These are the single source
// for code, value type, unit and permitted answers; the generated
// observation profile derives one invariant from each of them.
// The codes follow Santeon_MetingNaamCodelijst (the measurement names of
// the SIM AlgemeneMeting model).
// =====================================================================

Alias: $LOINC = http://loinc.org
Alias: $SCT = http://snomed.info/sct
Alias: $UCUM = http://unitsofmeasure.org
Alias: $ObsCategory = http://terminology.hl7.org/CodeSystem/observation-category


// --- quantity result -------------------------------------------------
Instance: od-calprotectin
InstanceOf: ObservationDefinition
Usage: #definition
Title: "Calprotectine (feces)"
Description: "Faecal calprotectin, numeric result in microgram per gram."
* category = $ObsCategory#laboratory
* code = $LOINC#38445-3 "Calprotectin [Mass/volume] in Stool"
* preferredReportName = "Calprotectine (feces)"
* permittedDataType = #Quantity
* quantitativeDetails.unit = $UCUM#ug/g "microgram per gram"


// --- coded result ----------------------------------------------------
Instance: od-alcohol-use
InstanceOf: ObservationDefinition
Usage: #definition
Title: "Alcoholgebruik"
Description: "Alcohol use status; the answer is a code from the valid coded value set."
* category = $ObsCategory#social-history
* code = $SCT#228273003 "Finding relating to alcohol drinking behavior (finding)"
* preferredReportName = "Alcoholgebruik"
* permittedDataType = #CodeableConcept
// The answers are the zib value set AlcoholGebruikStatusCodelijst, maintained in ART-DECOR.
// It is referenced by its canonical URL and registered in external-valuesets.yaml (key
// alcohol-use-status), which pins the version that the generated profile uses.
* validCodedValueSet.reference = "http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.40.2.7.3.2"

Instance: od-smoking-status
InstanceOf: ObservationDefinition
Usage: #definition
Title: "Rookstatus"
Description: "Tobacco smoking status (illustrative); the answers come from the Nationale Terminologieserver. No KPI uses this measurement yet, so it is not part of the generated model."
* category = $ObsCategory#social-history
* code = $LOINC#72166-2 "Tobacco smoking status"
* preferredReportName = "Rookstatus"
* permittedDataType = #CodeableConcept
// The answers are a SNOMED ECL query on the NTS (smoker, non-smoker, ex-smoker and their
// descendants). Such a set cannot be downloaded, so it is registered in external-valuesets.yaml
// (key smoking-status-nts) as reference only; the terminology server resolves it.
* validCodedValueSet.reference = "http://snomed.info/sct?fhir_vs=ecl/%3C%3C%2077176002%20OR%20%3C%3C%208392000%20OR%20%3C%3C%208517006"

Instance: od-smoking-status
InstanceOf: ObservationDefinition
Usage: #definition
Title: "Rookstatus"
Description: "Tobacco smoking status (illustrative); the answers come from the Nationale Terminologieserver. No KPI uses this measurement yet, so it is not part of the generated model."
* category = $ObsCategory#social-history
* code = $LOINC#72166-2 "Tobacco smoking status"
* preferredReportName = "Rookstatus"
* permittedDataType = #CodeableConcept
// The answers are a SNOMED ECL query on the NTS (smoker, non-smoker, ex-smoker and their
// descendants). Such a set cannot be downloaded, so it is registered in external-valuesets.yaml
// (key smoking-status-nts) as reference only; the terminology server resolves it.
* validCodedValueSet.reference = "http://snomed.info/sct?fhir_vs=ecl/%3C%3C%2077176002%20OR%20%3C%3C%208392000%20OR%20%3C%3C%208517006"
