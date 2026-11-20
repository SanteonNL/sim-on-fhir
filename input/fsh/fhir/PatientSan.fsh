Profile: PatientSan
Parent: http://ig.nictiz.nl/gbb/StructureDefinition/nl-core-Patient
Id: patient-san
Title: "Patient (SAN)"
Description: "Generiek Santeon-profiel voor patiënten (SIMPatient), gebaseerd op Nictiz nl-core-Patient. Datasetprofielen sluiten de elementen die zij niet gebruiken."

// Opgenomen
* identifier 1..* MS
* identifier.value 1..1
* gender 0..1 MS
* birthDate 0..1 MS
* deceased[x] 0..1 MS
* deceased[x] only dateTime

// Uitgesloten
* identifier[bsn] 0..0
* implicitRules 0..0
* text 0..0
* contained 0..0
* modifierExtension 0..0


Mapping: PatientSanFromSIMPatient
Id: sim
Title: "Santeon Informatiemodel (SIMPatient)"
Description: "Herkomst van de PatientSan-elementen in het logisch model SIMPatient."
Source: PatientSan
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMPatient"

* identifier -> "SIMPatient.Identificatienummer"
* gender -> "SIMPatient.Geslacht" "Terminologiemapping van SIM GeslachtCodelijst (zib 2020) naar FHIR AdministrativeGender."
* birthDate -> "SIMPatient.Geboortedatum"
* deceasedDateTime -> "SIMPatient.DatumOverlijden" "SIM date als dateTime met dagprecisie."
