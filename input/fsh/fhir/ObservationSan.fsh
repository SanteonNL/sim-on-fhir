
Profile: ObservationSan
Parent: Observation
Id: observation-san
Title: "Observation (SAN)"
Description: """
Generiek Santeon FHIR R4-profiel voor algemene metingen.

Gebaseerd op het Santeon Informatiemodel (SIMAlgemeneMeting).

De-identificatie wordt uitgevoerd volgens afzonderlijke
transformatiespecificaties en is geen inherente
eigenschap van dit profiel.
"""

// Identifier (SIMAlgemeneMeting.MetingID)
* identifier 1..1
* identifier.use 0..0
* identifier.system 0..0
* identifier.value 1..1

// Observation.status is verplicht in FHIR R4.
// TODO: herkomst of gevalideerde afleidingsregel vaststellen.

// MetingNaam
* code 1..1
* code from Santeon_MetingNaamCodelijst (required)
* code.coding 1..*
* code.coding.system 1..1
* code.coding.code 1..1
* code.coding.display 0..1

// PatientIdentificatienummer
* subject 1..1
* subject only Reference(PatientSan)

// MetingDatumTijd
* effective[x] 0..1
* effective[x] only dateTime

// Uitslag: vier mogelijke datatypes.
// FHIR value[x] staat maximaal één type toe.
* value[x] 0..1
* value[x] only Quantity or Range or CodeableConcept or dateTime

// Meetmethode
* method 0..1

// Overige FHIR-elementen blijven beschikbaar.
// Toepassingsspecifieke beperkingen kunnen
// in een afgeleid profiel worden opgenomen.


Mapping: ObservationSanFromSIMCSV
Id: sim
Title: "Santeon Informatiemodel (SIM CSV)"
Description: "Informatieve mapping van FHIR Observation naar SIM CSV-bronvelden."
Source: ObservationSan
Target: "urn:santeon:sim:csv"

// Identificatie
// Identificatie
* identifier.system -> "AlgemeneMeting;MetingID" "Systeemdeel van de samengestelde identifier."
* identifier.value -> "AlgemeneMeting;MetingID" "Waardedeel van de samengestelde identifier; hash."

* subject -> "AlgemeneMeting;Identificatienummer" "Patient-reference op basis van patiëntidentifier."

// MetingID stond in het eerdere FHIR-profiel,
// maar niet in het huidige SIM Logical Model.
// Eerst controleren in de volledige SIM CSV.
// * identifier.value -> "AlgemeneMeting;MetingID"

// Meting
* code.coding.system -> "AlgemeneMeting;MetingNaamCodeSysteem"
* code.coding.code -> "AlgemeneMeting;MetingNaamCode"
* code.coding.display -> "AlgemeneMeting;MetingNaamOmschrijving"
* effectiveDateTime -> "AlgemeneMeting;MetingDatumTijd"

// Numerieke uitslag
* valueQuantity.value -> "AlgemeneMeting;UitslagWaarde"
* valueQuantity.system -> "AlgemeneMeting;UitslagWaardeEenheidSysteem"
* valueQuantity.code -> "AlgemeneMeting;UitslagWaardeEenheid"
* valueQuantity.comparator -> "AlgemeneMeting;UitslagWaardeOperator"

// Uitslag als bereik
* valueRange.low.value -> "AlgemeneMeting;ObservationValueRangeLow"
* valueRange.low.unit -> "AlgemeneMeting;ObservationValueRangeLowUnit"
* valueRange.low.system -> "AlgemeneMeting;ObservationValueRangeLowSystem"
* valueRange.low.code -> "AlgemeneMeting;ObservationValueRangeLowCode"

* valueRange.high.value -> "AlgemeneMeting;ObservationValueRangeHigh"
* valueRange.high.unit -> "AlgemeneMeting;ObservationValueRangeHighUnit"
* valueRange.high.system -> "AlgemeneMeting;ObservationValueRangeHighSystem"
* valueRange.high.code -> "AlgemeneMeting;ObservationValueRangeHighCode"

// Gecodeerde uitslag
* valueCodeableConcept.coding.system -> "AlgemeneMeting;UitslagCodeSysteem"
* valueCodeableConcept.coding.code -> "AlgemeneMeting;UitslagCode"
* valueCodeableConcept.coding.display -> "AlgemeneMeting;UitslagCodeOmschrijving"

// Datum/tijd als uitslag
* valueDateTime -> "AlgemeneMeting;UitslagDatumTijd"

// Meetmethode
* method.coding.system -> "AlgemeneMeting;MeetmethodeCodeSysteem"
* method.coding.code -> "AlgemeneMeting;MeetmethodeCode"
* method.coding.display -> "AlgemeneMeting;MeetmethodeOmschrijving"


// Nog uit te werken:
// Observation.status: verplicht in FHIR R4.
// Maximaal één value[x]-vorm kiezen bij transformatie.
// Toegestane uitslagtypen en ValueSets per MetingNaam
// later via ObservationDefinition.