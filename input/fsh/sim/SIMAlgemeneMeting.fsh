Logical: SIMAlgemeneMeting
Parent: Base
Id: SIMAlgemeneMeting
Title: "Algemene meting (model)"
Description: "Logisch model voor een algemeneMeting binnen het Santeon Informatiemodel."

* MetingNaam 1..1 CodeableConcept "Type of naam van de meting"
* MetingNaam from Santeon_MetingNaamCodelijst (required)
* MetingNaam obeys validatie-metingnaam-uitslagcode
* MetingDatumTijd 0..1 dateTime "Datum en eventueel tijd die het dichtst ligt bij het daadwerkelijke meetmoment of de observatie"
* UitslagWaarde 0..1 Quantity "Numerieke uitslag van de meting, met eenheid (UCUM) en eventueel een vergelijkingsoperator (<, <=, >=, >)"
* UitslagWaarde obeys validatie-metingnaam-uitslagcode
* UitslagBereik 0..1 Range "Uitslag als bereik: een ondergrens en/of een bovengrens, beide inclusief"
* UitslagCode 0..1 CodeableConcept "Gecodeerde uitslag van de meting"
* UitslagCode ^definition = "Welke ValueSet van toepassing is op UitslagCode wordt bepaald door MetingNaam, zie invariants."
* UitslagCode obeys validatie-metingnaam-uitslagcode
* UitslagDatumTijd 0..1 dateTime "Datum en eventueel tijd als antwoord op een vraag, bijvoorbeeld: sinds wanneer rookt u? Een vage datumtijd (alleen jaar of datum) is niet wenselijk maar wel toegestaan."

// Een meting heeft één uitslag: in FHIR is dat Observation.value[x] met één van deze typen
// (Quantity, Range, CodeableConcept, dateTime).


// Validatieregel of Invariants
Invariant: validatie-metingnaam-uitslagcode
Description: "Valideert de relatie tussen MetingNaam en het bijbehorende type uitslag. Voor metingen met een gecodeerde uitslag moet UitslagCode afkomstig zijn uit de ValueSet die aan de betreffende MetingNaam is gekoppeld. Bijvoorbeeld: wanneer MetingNaam alcoholgebruik is (SNOMED CT 228273003), moet UitslagCode afkomstig zijn uit AlcoholGebruikStatusCodelijst. Dit geldt voor alle vastgelegde [MetingNaam-UitslagCode](<a href='https://dev.azure.com/SanteonNL/Santeon/_git/HipsETL?path=/SIM/informatiemodel%20Santeon%20valueSets%20relation.csv</a>)-combinaties. Metingen die niet in deze combinaties zijn vastgelegd, maar wel voorkomen in Santeon_MetingNaamCodelijst, hebben een UitslagWaarde als uitkomst."
Severity: #error
Expression: "(MetingNaam.coding.where(system = 'http://snomed.info/sct' and code = '228273003').exists() implies UitslagCode.memberOf('https://ig.santeon.nl/sim-on-fhir/ValueSet/AlcoholGebruikStatusCodelijst')) or (MetingNaam.coding.where(system = 'http://loinc.org' and code = '38445-3').exists() implies UitslagWaarde.exists())"

// Mappings
Mapping: SIMAlgemeneMetingFromSIMCSV
Id: SIMAlgemeneMetingFromSIMCSV
Title: "SIM CSVs"
Source: SIMAlgemeneMeting
Target: "SIM CSVs"

* MetingNaam.coding.system -> "MetingNaamCodeSysteem"
* MetingNaam.coding.code -> "MetingNaamCode"
* MetingNaam.coding.display -> "MetingNaamOmschrijving"
* MetingDatumTijd -> "MetingDatumTijd"
* UitslagWaarde.value -> "UitslagWaarde"
* UitslagWaarde.system -> "UitslagWaardeEenheidSysteem"
* UitslagWaarde.code -> "UitslagWaardeEenheid"
* UitslagWaarde.comparator -> "UitslagWaardeOperator"
* UitslagBereik.low.value -> "ObservationValueRangeLow"
* UitslagBereik.low.unit -> "ObservationValueRangeLowUnit"
* UitslagBereik.low.system -> "ObservationValueRangeLowSystem"
* UitslagBereik.low.code -> "ObservationValueRangeLowCode"
* UitslagBereik.high.value -> "ObservationValueRangeHigh"
* UitslagBereik.high.unit -> "ObservationValueRangeHighUnit"
* UitslagBereik.high.system -> "ObservationValueRangeHighSystem"
* UitslagBereik.high.code -> "ObservationValueRangeHighCode"
* UitslagCode.coding.system -> "UitslagCodeSysteem"
* UitslagCode.coding.code -> "UitslagCode"
* UitslagCode.coding.display -> "UitslagCodeOmschrijving"
* UitslagDatumTijd -> "UitslagDatumTijd"

Mapping: SIMAlgemeneMetingToFHIR
Id: SIMAlgemeneMetingToFHIR
Title: "FHIR"
Source: SIMAlgemeneMeting
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/observation-san"
// "http://hl7.org/fhir/StructureDefinition/observation-san"

* MetingNaam -> "Observation.code"
* MetingDatumTijd -> "Observation.effectiveDateTime"
* UitslagWaarde -> "Observation.valueQuantity"
* UitslagBereik -> "Observation.valueRange"
* UitslagCode -> "Observation.valueCodeableConcept"
* UitslagDatumTijd -> "Observation.valueDateTime"