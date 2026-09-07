Logical: SIMFasering
Id: SIMFasering
Title: "SIM Fasering"
Description: "Logisch model voor een procesfasering binnen het Santeon Informatiemodel, bijvoorbeeld faseringen rondom een operatie."

* FaseringID 1..1 Identifier "Unieke identificatie van de fasering"
* ContactID 0..1 Identifier "Identificatie van het contact waarbinnen de fasering plaatsvindt"
* OperatieID 0..1 Identifier "Identificatie van de operatie waarop de fasering betrekking heeft"
* Patient 1..1 Identifier "Identificatie van de patiënt"

* FaseringType 1..1 CodeableConcept "Type procesfasering"
// * FaseringType from Santeon_FaseringCodelijst (required)

* BeginDatumTijd 1..1 dateTime "Datum en tijd waarop de fasering begint"
* EindDatumTijd 1..1 dateTime "Datum en tijd waarop de fasering eindigt"

// Mappings
Mapping: SIMFaseringFromSIMCSV
Id: SIMFaseringFromSIMCSV
Title: "SIM CSVs"
Source: SIMFasering
Target: "SIM CSVs"

* FaseringID.system -> "FaseringID_system"
* FaseringID.value -> "FaseringID_value"

* ContactID.system -> "ContactID_system"
* ContactID.value -> "ContactID_value"

* OperatieID.system -> "OperatieID_system"
* OperatieID.value -> "OperatieID_value"

* Patient.system -> "Identificatienummer_system"
* Patient.value -> "Identificatienummer_value"

* FaseringType.coding.system -> "FaseringCodeSysteem"
* FaseringType.coding.code -> "FaseringCode"
* FaseringType.coding.display -> "FaseringOmschrijving"

* BeginDatumTijd -> "FaseringBeginDatumTijd"
* EindDatumTijd -> "FaseringEindDatumTijd"

Mapping: SIMFaseringToFHIR
Id: SIMFaseringToFHIR
Title: "SIM Fasering naar FHIR"
Source: SIMFasering
Target: "http://hl7.org/fhir/StructureDefinition/EncounterSan"

* FaseringID -> "Encounter.identifier"
* ContactID -> "Encounter.partOf"
* Patient -> "Encounter.subject"
* FaseringType -> "Encounter.type"
* BeginDatumTijd -> "Encounter.period.start"
* EindDatumTijd -> "Encounter.period.end"

// Nog te bepalen hoe de relatie met de operatie (Procedure) wordt gemodelleerd