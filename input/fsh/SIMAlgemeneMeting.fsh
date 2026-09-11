Logical: SIMAlgemeneMeting
Id: SIMAlgemeneMeting
Title: "CLM SIMAlgemeneMeting"
Description: "Logisch model voor een algemeneMeting binnen het Santeon Informatiemodel."

* MetingNaam 1..1 CodeableConcept "Type of naam van de meting"
* MetingNaam from Santeon_MetingNaamCodelijst (required)
* MetingNaam obeys validatie-metingnaam-uitslagcode
* MetingDatumTijd 0..1 dateTime "Datum en eventueel tijd die het dichtst ligt bij het daadwerkelijke meetmoment of de observatie"
* UitslagWaarde 0..1 Quantity "Numerieke uitslag van de meting"
* UitslagWaarde obeys validatie-metingnaam-uitslagcode
* UitslagCode 0..1 CodeableConcept "Gecodeerde uitslag van de meting"
* UitslagCode ^definition = "Welke ValueSet van toepassing is op UitslagCode wordt bepaald door MetingNaam, zie invariants."
* UitslagCode obeys validatie-metingnaam-uitslagcode


// Validatieregel of Invariants
Invariant: validatie-metingnaam-uitslagcode
Description: "Valideert de relatie tussen MetingNaam en het bijbehorende type uitslag. Voor metingen met een gecodeerde uitslag moet UitslagCode afkomstig zijn uit de ValueSet die aan de betreffende MetingNaam is gekoppeld. Bijvoorbeeld: wanneer MetingNaam alcoholgebruik is (SNOMED CT 228273003), moet UitslagCode afkomstig zijn uit AlcoholGebruikStatusCodelijst. Dit geldt voor alle vastgelegde [MetingNaam-UitslagCode](<a href='https://dev.azure.com/SanteonNL/Santeon/_git/HipsETL?path=/SIM/informatiemodel%20Santeon%20valueSets%20relation.csv</a>)-combinaties. Metingen die niet in deze combinaties zijn vastgelegd, maar wel voorkomen in Santeon_MetingNaamCodelijst, hebben een UitslagWaarde als uitkomst."
Severity: #error
Expression: "(MetingNaam.coding.where(system = 'http://snomed.info/sct' and code = '228273003').exists() implies UitslagCode.memberOf('https://ig.santeon.nl/ibd/ValueSet/AlcoholGebruikStatusCodelijst')) or (MetingNaam.coding.where(system = 'http://loinc.org' and code = '38445-3').exists() implies UitslagWaarde.exists())"

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
* UitslagCode.coding.system -> "UitslagCodeSysteem"
* UitslagCode.coding.code -> "UitslagCode"
* UitslagCode.coding.display -> "UitslagCodeOmschrijving"

Mapping: SIMAlgemeneMetingToFHIR
Id: SIMAlgemeneMetingToFHIR
Title: "FHIR"
Source: SIMAlgemeneMeting
Target: "https://ig.santeon.nl/ibd/StructureDefinition/ObservationSan"
// "http://hl7.org/fhir/StructureDefinition/ObservationSan"

* MetingNaam -> "Observation.code"
* MetingDatumTijd -> "Observation.effectiveDateTime"
* UitslagCode -> "Observation.valueCodeableConcept"