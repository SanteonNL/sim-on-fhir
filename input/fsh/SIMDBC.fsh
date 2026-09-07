Logical: SIMDBC
Id: SIMDBC
Title: "SIM DBC"
Description: "Logisch model voor een DBC binnen het Santeon Informatiemodel."

* OpeningsDatum 0..1 dateTime "Openingsdatum van de DBC"
* SpecialismeDiagnose 0..1 CodeableConcept "Specialisme en diagnose behorend bij de DBC"

Mapping: SIMDBCFromSIMCSV
Id: SIMDBCFromSIMCSV
Title: "SIM CSVs"
Source: SIMDBC
Target: "SIM CSVs"

* OpeningsDatum -> "DBCOpeningsDatum"
* SpecialismeDiagnose.coding.system -> "SpecialismeDiagnoseSysteem"
* SpecialismeDiagnose.coding.code -> "SpecialismeDiagnoseCode"
* SpecialismeDiagnose.coding.display -> "SpecialismeDiagnoseCodeOmschrijving"

Mapping: SIMDBCToFHIR
Id: SIMDBCToFHIR
Title: "SIM DBC naar FHIR"
Source: SIMDBC
Target: "http://hl7.org/fhir/StructureDefinition/EpisodeOfCareSan"

* OpeningsDatum -> "EpisodeOfCare.period.start"
* SpecialismeDiagnose -> "Condition.code; referenced by EpisodeOfCare.diagnosis.condition"