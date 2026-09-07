Logical: SIMAlgemeneMeting
Id: SIMAlgemeneMeting
Title: "SIM AlgemeneMeting"
Description: "Logisch model voor een algemeneMeting binnen het Santeon Informatiemodel."

* MetingNaam 1..1 CodeableConcept "Type of naam van de meting"
* MetingNaam from Santeon_MetingNaamCodelijst (required)
* MetingDatumTijd 0..1 dateTime "Datum en eventueel tijd die het dichtst ligt bij het daadwerkelijke meetmoment of de observatie"
* UitslagCode 0..1 CodeableConcept "Gecodeerde uitslag van de meting"
* UitslagCode from AlcoholGebruikStatusCodelijst (example)

// Mappings
// * ^mapping[+].identity = "sim-csv"
// * ^mapping[=].name = "SIM CSVs"

// * ^mapping[+].identity = "santeon-fhir"
// * ^mapping[=].name = "SIMAlgemeneMeting naar FHIR"

// MetingNaam -> FHIR
// * MetingNaam ^mapping[1].identity = "santeon-fhir"
// * MetingNaam ^mapping[1].map = "Observation.code"

// MetingNaam -> CSV
// * MetingNaam.coding.system ^mapping[0].identity = "sim-csv"
// * MetingNaam.coding.system ^mapping[0].map = "MetingNaamCodeSysteem"

// * MetingNaam.coding.code ^mapping[0].identity = "sim-csv"
// * MetingNaam.coding.code ^mapping[0].map = "MetingNaamCode"

// * MetingNaam.coding.display ^mapping[0].identity = "sim-csv"
// * MetingNaam.coding.display ^mapping[0].map = "MetingNaamOmschrijving"

// MetingDatumTijd -> CSV + FHIR
// * MetingDatumTijd ^mapping[0].identity = "sim-csv"
// * MetingDatumTijd ^mapping[0].map = "MetingDatumTijd"

// * MetingDatumTijd ^mapping[1].identity = "santeon-fhir"
// * MetingDatumTijd ^mapping[1].map = "Observation.effectiveDateTime"

// UitslagCode -> FHIR
// * UitslagCode ^mapping[1].identity = "santeon-fhir"
// * UitslagCode ^mapping[1].map = "Observation.valueCodeableConcept"

// UitslagCode -> CSV
// * UitslagCode.coding.system ^mapping[0].identity = "sim-csv"
// * UitslagCode.coding.system ^mapping[0].map = "UitslagCodeSysteem"

// * UitslagCode.coding.code ^mapping[0].identity = "sim-csv"
// * UitslagCode.coding.code ^mapping[0].map = "UitslagCode"

// * UitslagCode.coding.display ^mapping[0].identity = "sim-csv"
// * UitslagCode.coding.display ^mapping[0].map = "UitslagCodeOmschrijving"

// Mappings van SIM CSV
Mapping: SIMAlgemeneMetingFromSIMCSV
Id: SIMAlgemeneMetingFromSIMCSV
Title: "SIM CSVs"
Source: SIMAlgemeneMeting
Target: "SIM CSVs"

* MetingNaam.coding.system -> "MetingNaamCodeSysteem"
* MetingNaam.coding.code -> "MetingNaamCode"
* MetingNaam.coding.display -> "MetingNaamOmschrijving"
* MetingDatumTijd -> "MetingDatumTijd"
* UitslagCode.coding.system -> "UitslagCodeSysteem"
* UitslagCode.coding.code -> "UitslagCode"
* UitslagCode.coding.display -> "UitslagCodeOmschrijving"

// Mappings naar SIM FHIR
Mapping: SIMAlgemeneMetingToFHIR
Id: SIMAlgemeneMetingToFHIR
Title: "SIMAlgemeneMeting naar FHIR"
Source: SIMAlgemeneMeting
Target: "https://ig.santeon.nl/ibd/StructureDefinition/ObservationSan"

* MetingNaam -> "Observation.code"
* MetingDatumTijd -> "Observation.effectiveDateTime"
* UitslagCode -> "Observation.valueCodeableConcept"