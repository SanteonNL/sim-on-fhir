// ============================================================
// IBD Patient Profiel
// Cohort: patiënten met IBD-gerelateerde DOT of CarePlan categorie
// Exclusie: patiënten jonger dan 18 jaar
// ============================================================

Profile: IBDPatient
Parent: Patient
Id: ibd-patient
Title: "IBD Patient"
Description: """
  Profiel voor patiënten in de IBD-populatie. 
  Patiënten worden geïncludeerd op basis van:
  - Een DOT in specialisme 0313 (Interne Geneeskunde/MDL) met diagnosecode 922 of 923, of
  - Een DOT in specialisme 0318 (MDL) met diagnosecode 601 of 602.
  Patiënten jonger dan 18 jaar op het moment van aanlevering zijn geëxcludeerd.
"""

* identifier 1..* MS
* identifier ^slicing.discriminator.type = #value
* identifier ^slicing.discriminator.path = "system"
* identifier ^slicing.rules = #open
* identifier contains patientnummer 1..1 MS

* identifier[patientnummer].system 1..1 MS
* identifier[patientnummer].system = "https://santeon.nl/fhir/NamingSystem/patientnummer"
* identifier[patientnummer].value 1..1 MS

* birthDate 1..1 MS
* birthDate ^short = "Geboortedatum — gebruikt voor leeftijdscheck (≥18 jaar)"

* gender MS

// ============================================================
// IBD EpisodeOfCare Profiel (DOT / Subtraject)
// ============================================================

Profile: IBDEpisodeOfCare
Parent: EpisodeOfCare
Id: ibd-episode-of-care
Title: "IBD EpisodeOfCare (DOT/Subtraject)"
Description: """
  Profiel voor een IBD-gerelateerd DBC-subtraject (DOT).
  Bevat specialismecode (0313/0318), diagnosecode en zorgtype (11 of 21).
  De periode start op of na 01-01-{jaar-7}.
"""

* identifier MS
* status 1..1 MS

* type 1..* MS
* type ^short = "Specialisme + diagnosecode combinatie"

// Specialisme als extensie (NZa/AGB-Z specialismecode)
* extension contains IBDSpecialisme named specialisme 1..1 MS
* extension contains IBDDiagnoseCode named diagnoseCode 1..1 MS
* extension contains IBDZorgtype named zorgtype 1..1 MS

* patient 1..1 MS
* patient only Reference(IBDPatient)

* period 1..1 MS
* period.start 1..1 MS
* period.start ^short = "BEGINDAT van het subtraject (DOT openingsdatum)"

// ============================================================
// Extensies voor DOT-specifieke velden
// ============================================================

Extension: IBDSpecialisme
Id: ibd-specialisme
Title: "IBD Specialisme"
Description: "NZa-specialismecode van de uitvoerder (bijv. 0313, 0318)"
* value[x] only Coding
* valueCoding.system = "https://santeon.nl/fhir/CodeSystem/nza-specialisme"
* valueCoding from IBDSpecialismeVS (required)

Extension: IBDDiagnoseCode
Id: ibd-diagnose-code
Title: "IBD Diagnose Code"
Description: "DBC-diagnosecode (bijv. 601, 602, 922, 923)"
* value[x] only Coding
* valueCoding.system = "https://santeon.nl/fhir/CodeSystem/dbc-diagnose"
* valueCoding from IBDDiagnoseVS (required)

Extension: IBDZorgtype
Id: ibd-zorgtype
Title: "IBD Zorgtype"
Description: "Landelijke zorgtypecode — alleen 11 (regulier) en 21 (herhaling) zijn geldig"
* value[x] only Coding
* valueCoding.system = "https://santeon.nl/fhir/CodeSystem/nza-zorgtype"
* valueCoding from IBDZorgtypeVS (required)

// ============================================================
// ValueSets
// ============================================================

ValueSet: IBDSpecialismeVS
Id: ibd-specialisme-vs
Title: "IBD Specialisme ValueSet"
Description: "Geldige NZa-specialismecodes voor IBD-cohort"
* ^status = #active
* include codes from system https://santeon.nl/fhir/CodeSystem/nza-specialisme
    where concept is-a #0313  // Interne Geneeskunde (MDL)
* include codes from system https://santeon.nl/fhir/CodeSystem/nza-specialisme
    where concept is-a #0318  // Maag-Darm-Leverziekten

ValueSet: IBDDiagnoseVS
Id: ibd-diagnose-vs
Title: "IBD Diagnose ValueSet"
Description: "Geldige DBC-diagnosecodes voor IBD-cohort"
* ^status = #active
* https://santeon.nl/fhir/CodeSystem/dbc-diagnose#601 "Colitis ulcerosa"
* https://santeon.nl/fhir/CodeSystem/dbc-diagnose#602 "Ziekte van Crohn"
* https://santeon.nl/fhir/CodeSystem/dbc-diagnose#922 "IBD (specialisme 0313 variant A)"
* https://santeon.nl/fhir/CodeSystem/dbc-diagnose#923 "IBD (specialisme 0313 variant B)"

ValueSet: IBDZorgtypeVS
Id: ibd-zorgtype-vs
Title: "IBD Zorgtype ValueSet"
Description: "Toegestane zorgtypes voor IBD-subtrajecten (landelijk 11 en 21)"
* ^status = #active
* https://santeon.nl/fhir/CodeSystem/nza-zorgtype#11 "Regulier zorgtype"
* https://santeon.nl/fhir/CodeSystem/nza-zorgtype#21 "Herhaling zorgtype"
