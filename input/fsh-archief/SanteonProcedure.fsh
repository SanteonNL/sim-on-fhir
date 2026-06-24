// ============================================================
// Santeon Procedure Profile (Verrichting)
// IG:     santeon.ibd.ig
// Base:   http://hl7.org/fhir/StructureDefinition/Procedure (R4)
// ZIB:    Verrichting-v5.1(2019NL)
//
// Used in IBD indicator K3.6.3: scopie procedures
// De-identification notes are in comments per element.
// ============================================================

Profile: SanteonProcedure
Parent: Procedure
Id: santeon-procedure
Title: "Santeon Procedure (Verrichting)"
Description: """
  Santeon-internal Procedure profile derived from base FHIR R4,
  mapped to ZIB Verrichting-v5.1(2019NL).

  Used for procedures including scopie (NZa codes 034620, 034686,
  034690, 035582) in the IBD use case (indicator K3.6.3).

  De-identification strategy per element:
  - subject.identifier     → hash
  - performedPeriod.start  → dateshift
"""

// === SQL Data Source Mapping ===

* ^mapping[+].identity = "hips-procedure-sql"
* ^mapping[=].name = "SQL Data Mapping (hips_procedure.sql)"
* ^mapping[=].uri = "urn:santeon:mapping:hips-procedure-sql"
* ^mapping[=].comment = "Maps Procedure elements to hips_procedure.sql"

* code ^mapping[0].identity = "hips-procedure-sql"
* code ^mapping[0].map = "Verrichting.VerrichtingTypeCodeNZa\nVerrichting.VerrichtingTypeCodeSnomedCT\nVerrichting.VerrichtingTypeCodeDHD"
* code ^mapping[0].comment = "Procedure code from three coding systems: NZa, SNOMED CT, DHD"

* subject ^mapping[0].identity = "hips-procedure-sql"
* subject ^mapping[0].map = "Verrichting.Identificatienummer"
* subject ^mapping[0].comment = "Patient identifier (BSN), de-identified via hash"

* performed[x] ^mapping[0].identity = "hips-procedure-sql"
* performed[x] ^mapping[0].map = "Verrichting.VerrichtingStartDatum"
* performed[x] ^mapping[0].comment = "Procedure start date, de-identified via dateshift"

* performer ^mapping[0].identity = "hips-procedure-sql"
* performer ^mapping[0].map = "Verrichting.Uitvoerder_Specialisme"
* performer ^mapping[0].comment = "Performer specialty (AGB code)"

* basedOn ^mapping[0].identity = "hips-procedure-sql"
* basedOn ^mapping[0].map = "Verrichting.Aanvrager_Specialisme"
* basedOn ^mapping[0].comment = "Requester specialty via ServiceRequest.requester (AGB code)"

// -----------------------------------------------------------
// Suppress meta/narrative overhead
// -----------------------------------------------------------

* meta 0..0
* implicitRules 0..0
* language 0..0
* text 0..0
* contained 0..0
* extension 0..0
* modifierExtension 0..0

// ============================================================
// [1] identifier  (optional, for procedure-level ID)
// ============================================================
* identifier 0..*
* identifier.id 0..0
* identifier.use 0..0
* identifier.type 0..0
* identifier.period 0..0
* identifier.assigner 0..0

// ============================================================
// [2] status  (required=true, base 1..1 — unchanged)
// preparation | in-progress | not-done | on-hold | stopped |
// completed | entered-in-error | unknown
// ============================================================


// ============================================================
// [3] code  (required=true, VerrichtingTypeCode)
// Maps to ZIB Verrichting VerrichtingTypeCode
// Three coding systems: NZa, SNOMED CT, DHD
// NOTE: OIDs should be verified against Nictiz/Santeon registry
// ============================================================
* code 1..1
* code.id 0..0
* code.text 0..0
* code.coding 1..*
* code.coding ^slicing.discriminator.type = #value
* code.coding ^slicing.discriminator.path = "system"
* code.coding ^slicing.rules = #open
* code.coding contains nza 0..1 and snomed 0..1 and dhd 0..1

* code.coding[nza] ^mapping[0].identity = "hips-procedure-sql"
* code.coding[nza] ^mapping[0].map = "Verrichting.VerrichtingTypeCodeNZa"
* code.coding[nza] ^mapping[0].comment = "NZa verrichtingencode"
* code.coding[nza].system 1..1
* code.coding[nza].system = "urn:oid:2.16.840.1.113883.2.4.6.14"
* code.coding[nza].code 1..1
* code.coding[nza].display 0..1
* code.coding[nza].id 0..0
* code.coding[nza].version 0..0
* code.coding[nza].userSelected 0..0

* code.coding[snomed] ^mapping[0].identity = "hips-procedure-sql"
* code.coding[snomed] ^mapping[0].map = "Verrichting.VerrichtingTypeCodeSnomedCT"
* code.coding[snomed] ^mapping[0].comment = "SNOMED CT procedure code"
* code.coding[snomed].system 1..1
* code.coding[snomed].system = "http://snomed.info/sct"
* code.coding[snomed].code 1..1
* code.coding[snomed].display 0..1
* code.coding[snomed].id 0..0
* code.coding[snomed].version 0..0
* code.coding[snomed].userSelected 0..0

* code.coding[dhd] ^mapping[0].identity = "hips-procedure-sql"
* code.coding[dhd] ^mapping[0].map = "Verrichting.VerrichtingTypeCodeDHD"
* code.coding[dhd] ^mapping[0].comment = "DHD verrichtingencode"
* code.coding[dhd].system 1..1
* code.coding[dhd].system = "urn:oid:2.16.840.1.113883.2.4.3.22.1.4"
* code.coding[dhd].code 1..1
* code.coding[dhd].display 0..1
* code.coding[dhd].id 0..0
* code.coding[dhd].version 0..0
* code.coding[dhd].userSelected 0..0

// ============================================================
// [4] subject  (required=true, deident=hash on Patient.identifier)
// Maps to ZIB Verrichting Identificatienummer
// ============================================================
* subject 1..1
* subject only Reference(Patient)

// ============================================================
// [5] performedPeriod  (required=true, deident=dateshift)
// Maps to ZIB Verrichting VerrichtingStartDatum
// ============================================================
* performed[x] only Period
* performed[x] 1..1
* performedPeriod.id 0..0
* performedPeriod.start 1..1
* performedPeriod.end 0..1

// ============================================================
// [6] performer  (optional, Uitvoerder_Specialisme)
// Maps to ZIB Zorgverlener Specialisme
// Full FHIR path: Procedure.performer.actor → Practitioner.qualification.code
// ============================================================
* performer 0..*
* performer.id 0..0
* performer.function 0..1
* performer.actor 1..1
* performer.actor only Reference(Practitioner)
* performer.onBehalfOf 0..0

// ============================================================
// [7] basedOn  (optional, Aanvrager_Specialisme via ServiceRequest)
// Maps to ZIB Zorgverlener Specialisme for the requester
// Full FHIR path: Procedure.basedOn → ServiceRequest.requester →
//   Practitioner.qualification.code
// ============================================================
* basedOn 0..*
* basedOn only Reference(ServiceRequest)

// ============================================================
// NOTE: VerrichtingAantal (procedure count) is available in the
// source data but has no standard FHIR mapping path.
// Consider a custom extension if needed in a future iteration.
// ============================================================

// -----------------------------------------------------------
// Suppress all other optional Procedure elements
// -----------------------------------------------------------
* instantiatesCanonical 0..0
* instantiatesUri 0..0
* partOf 0..0
* statusReason 0..0
* category 0..0
* encounter 0..0
* recorder 0..0
* asserter 0..0
* location 0..0
* reasonCode 0..0
* reasonReference 0..0
* bodySite 0..0
* outcome 0..0
* report 0..0
* complication 0..0
* complicationDetail 0..0
* followUp 0..0
* note 0..0
* focalDevice 0..0
* usedReference 0..0
* usedCode 0..0
