Logical: SIMAlgemeneMeting
Parent: Base
// Parent: GBB - nog te beoordelen
Id: SIMAlgemeneMeting
Title: "Algemene meting (model)"
Description: "Generiek logisch model voor een algemene meting binnen het Santeon Informatiemodel."

// IBD
* MetingID 1..1 Identifier "Uniek ID (key) binnen patiënt per meting"
* PatientIdentificatienummer 1..1 Identifier "Identificatienummer van de patiënt"

* MetingNaam 1..1 CodeableConcept "Type of naam van de meting"
* MetingNaam from Santeon_MetingNaamCodelijst (required)

* MetingDatumTijd 0..1 dateTime "Datum en eventueel tijd die het dichtst ligt bij het daadwerkelijke meetmoment of de observatie"

* UitslagWaarde 0..1 Quantity "Numerieke uitslag van de meting, met eenheid (UCUM) en eventueel een vergelijkingsoperator"

* UitslagBereik 0..1 Range "Uitslag als bereik: een ondergrens en/of een bovengrens, beide inclusief"

* UitslagCode 0..1 CodeableConcept "Gecodeerde uitslag van de meting"
* UitslagCode ^definition = "Welke ValueSet van toepassing is op UitslagCode wordt bepaald door MetingNaam. De bijbehorende terminologierelaties worden afzonderlijk beheerd."

* UitslagDatumTijd 0..1 dateTime "Datum en eventueel tijd als antwoord op een vraag, bijvoorbeeld: sinds wanneer rookt u? Een vage datumtijd (alleen jaar of datum) is niet wenselijk maar wel toegestaan."

* Meetmethode 0..1 CodeableConcept "Methode waarmee de meting is uitgevoerd"

// Een meting heeft maximaal één uitslagtype.
// FHIR: Observation.value[x] met Quantity, Range,
// CodeableConcept of dateTime.
//
// Validatie van toegestane uitslagtypen en ValueSets
// per MetingNaam wordt later uitgewerkt met
// ObservationDefinition en de SIM-terminologierelaties.


// Mapping vanuit SIM CSV
Mapping: SIMAlgemeneMetingFromSIMCSV
Id: SIMAlgemeneMetingFromSIMCSV
Title: "SIM CSV"
Source: SIMAlgemeneMeting
Target: "urn:santeon:sim:csv"

// Identificatie
* MetingID -> "AlgemeneMeting;MetingID"
* PatientIdentificatienummer -> "AlgemeneMeting;Identificatienummer"

// Meting
* MetingNaam.coding.system -> "AlgemeneMeting;MetingNaamCodeSysteem"
* MetingNaam.coding.code -> "AlgemeneMeting;MetingNaamCode"
* MetingNaam.coding.display -> "AlgemeneMeting;MetingNaamOmschrijving"
* MetingDatumTijd -> "AlgemeneMeting;MetingDatumTijd"

// Numerieke uitslag
* UitslagWaarde.value -> "AlgemeneMeting;UitslagWaarde"
* UitslagWaarde.system -> "AlgemeneMeting;UitslagWaardeEenheidSysteem"
* UitslagWaarde.code -> "AlgemeneMeting;UitslagWaardeEenheid"
* UitslagWaarde.comparator -> "AlgemeneMeting;UitslagWaardeOperator"

// Uitslag als bereik
* UitslagBereik.low.value -> "AlgemeneMeting;ObservationValueRangeLow"
* UitslagBereik.low.unit -> "AlgemeneMeting;ObservationValueRangeLowUnit"
* UitslagBereik.low.system -> "AlgemeneMeting;ObservationValueRangeLowSystem"
* UitslagBereik.low.code -> "AlgemeneMeting;ObservationValueRangeLowCode"
* UitslagBereik.high.value -> "AlgemeneMeting;ObservationValueRangeHigh"
* UitslagBereik.high.unit -> "AlgemeneMeting;ObservationValueRangeHighUnit"
* UitslagBereik.high.system -> "AlgemeneMeting;ObservationValueRangeHighSystem"
* UitslagBereik.high.code -> "AlgemeneMeting;ObservationValueRangeHighCode"

// Gecodeerde uitslag
* UitslagCode.coding.system -> "AlgemeneMeting;UitslagCodeSysteem"
* UitslagCode.coding.code -> "AlgemeneMeting;UitslagCode"
* UitslagCode.coding.display -> "AlgemeneMeting;UitslagCodeOmschrijving"

// Datum/tijd als uitslag
* UitslagDatumTijd -> "AlgemeneMeting;UitslagDatumTijd"

// Meetmethode
* Meetmethode.coding.system -> "AlgemeneMeting;MeetmethodeCodeSysteem"
* Meetmethode.coding.code -> "AlgemeneMeting;MeetmethodeCode"
* Meetmethode.coding.display -> "AlgemeneMeting;MeetmethodeOmschrijving"


// Mapping naar FHIR
Mapping: SIMAlgemeneMetingToFHIR
Id: SIMAlgemeneMetingToFHIR
Title: "SIMAlgemeneMeting naar FHIR"
Source: SIMAlgemeneMeting
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/observation-san"

// Identificatie
* MetingID -> "Observation.identifier"
* PatientIdentificatienummer -> "Observation.subject" "Patient-reference op basis van identificatienummer."

// Meting
* MetingNaam -> "Observation.code"
* MetingDatumTijd -> "Observation.effectiveDateTime"

// Uitslag
* UitslagWaarde -> "Observation.valueQuantity"
* UitslagBereik -> "Observation.valueRange"
* UitslagCode -> "Observation.valueCodeableConcept"
* UitslagDatumTijd -> "Observation.valueDateTime"

// Meetmethode
* Meetmethode -> "Observation.method"

// Nog uit te werken:
// Observation.status (FHIR verplicht)
// Selectie van maximaal één Observation.value[x]
// Conditionele terminologie via ObservationDefinition
// Meetmethode: codeersysteem en ValueSet controleren

// GBB: geen overeenkomstig Logical Model beschikbaar
// in Nictiz GBB 1.0.0-alpha.1.
// GBB-mapping toevoegen zodra een passend generiek
// Observation/Measurement-model is gepubliceerd.