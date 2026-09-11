Logical: SIMDBC
Id: SIMDBC
Title: "CLM SIMDBC/Zorgtraject"
Description: "Logisch model voor een DBC binnen het Santeon Informatiemodel."

* OpeningsDatum 0..1 dateTime "Openingsdatum van de DBC"
* SpecialismeDiagnose 0..1 CodeableConcept "Specialisme en diagnose behorend bij de DBC"
* SpecialismeDiagnose from https://ig.santeon.nl/ibd/ValueSet/Specialisme_DiagnoseCodelijstDummy (required)

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