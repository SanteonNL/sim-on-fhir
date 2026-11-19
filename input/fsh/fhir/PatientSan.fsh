Profile: PatientSan
Parent: Patient
// Parent: http://ig.nictiz.nl/gbb/StructureDefinition/nl-core-Patient
// nl-core wordt nog via een technische delta-mapping vergeleken.
Id: patient-san
Title: "Patient (SAN)"
Description: "Santeon FHIR R4-profiel voor patiëntgegevens, gebaseerd op het Santeon Informatiemodel (SIMPatient)."

// IBD-scope
* identifier 1..*
* identifier.value 1..1
* gender 0..1
* birthDate 0..1
* deceased[x] only dateTime

// Buiten IBD-scope - nog niet meegenomen
// Standaard FHIR-elementen blijven toegestaan.
// Eventuele toekomstige beperkingen worden alleen toegevoegd op basis van expliciete eisen.

// Terminologie
* gender from http://hl7.org/fhir/ValueSet/administrative-gender (required)



// ============================================
// MAPPING 1: Technische delta-analyse - PatientSan naar Nictiz nl-core
// ============================================

Mapping: PatientSanToNlCore
Id: PatientSanToNlCore
Title: "Nictiz nl-core-Patient"
Description: "Technische vergelijking tussen PatientSan en Nictiz nl-core Patient (referentieversie 1.0.0-alpha.2), inclusief toelichting op verschillen in FHIR-elementen, cardinaliteiten, datatypes en terminologiebindingen. Legenda: 🟢 Beoordeeld, equivalent of compatibel zonder relevante afwijking; 🟡 Verschil vastgesteld of nader te beoordelen."
Source: PatientSan
Target: "http://ig.nictiz.nl/gbb/StructureDefinition/nl-core-Patient"

// IBD-scope
* identifier -> "Patient.identifier" "🟡 Verschil in profilering: Nictiz definieert aanvullend een optionele BSN-slice 0..1."
* gender -> "Patient.gender" "🟢 Equivalent: beide gebruiken FHIR AdministrativeGender met required binding en cardinaliteit 0..1."
* birthDate -> "Patient.birthDate" "🟢 Equivalent: beide gebruiken FHIR date met cardinaliteit 0..1."
* deceased[x] -> "Patient.deceasedDateTime" "🟡 Verschil in representatie: PatientSan beperkt deceased[x] tot dateTime; nl-core ondersteunt daarnaast boolean."

// Buiten IBD-scope - nog te beoordelen
// -



// ============================================
// Status en openstaande controles
// ============================================

// Status: Ready for validation
// TODO: Controleer identifier-slicing met Nictiz.
// TODO: Beoordeel aanvullende nl-core obligations.

