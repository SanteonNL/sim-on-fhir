
Profile: ObservationSanTabakGebruik
Parent: ObservationSan
Id: observation-san-tabakgebruik
Title: "Observation TabakGebruik (SAN)"
Description: """
Santeon FHIR R4-profiel voor de vastgelegde rookstatus van een patiënt.

Afgeleid van ObservationSan en gebaseerd op het
Santeon Informatiemodel (SIMTabakGebruik).

De-identificatie wordt uitgevoerd volgens afzonderlijke
transformatiespecificaties en is geen inherente
eigenschap van dit profiel.
"""

// Identifier (SIMTabakGebruik.MetingID) en subject
// (PatientIdentificatienummer) zijn geërfd van ObservationSan.

// Observation.code
// De SIM-bron bevat geen code voor het type meting; de code ligt
// daarom vast in dit profiel.
// SNOMED CT 365980008 = "Bevinding betreffende tabakgebruik en
// blootstelling aan tabaksrook", dezelfde code die Nictiz
// zib-TobaccoUse (R4, zib2020) als pattern op Observation.code zet.
// De code staat ook in Santeon_MetingNaamCodelijst, waaraan
// ObservationSan de code verplicht bindt.
* code = http://snomed.info/sct#365980008

// DatumVastleggen
* effective[x] only dateTime

// TabakGebruikStatus
* value[x] only CodeableConcept
* value[x] from TabakGebruikStatusCodelijst (extensible)

// Een rookstatus heeft geen meetmethode.
* method 0..0


Mapping: ObservationSanTabakGebruikFromSIMCSV
Id: sim-tabakgebruik
Title: "Santeon Informatiemodel (SIM CSV) - TabakGebruik"
Description: "Informatieve mapping van FHIR Observation naar SIM TabakGebruik-bronvelden."
Source: ObservationSanTabakGebruik
Target: "urn:santeon:sim:csv"

// Identificatie
* identifier.system -> "TabakGebruik;MetingID" "Systeemdeel van de samengestelde identifier."
* identifier.value -> "TabakGebruik;MetingID" "Waardedeel van de samengestelde identifier; hash."

* subject -> "TabakGebruik;Identificatienummer" "Patient-reference op basis van patiëntidentifier."

// Rookstatus
* valueCodeableConcept.coding.system -> "TabakGebruik;TabakGebruikStatusSysteem"
* valueCodeableConcept.coding.code -> "TabakGebruik;TabakGebruikStatus"
* valueCodeableConcept.coding.display -> "TabakGebruik;TabakGebruikStatusOmschrijving"

* effectiveDateTime -> "TabakGebruik;TabakGebruikStatusDatumVastleggen" "Datum uit SIM; datumprecisie behouden."

// Nog uit te werken:
// Observation.status: verplicht in FHIR R4, geen SIM-bronkolom.
// TabakGebruikStatus: gebonden aan de SIM-versie van
// TabakGebruikStatusCodelijst; die wijkt op één code af van de
// Nictiz-lijst (zie de valueset).
