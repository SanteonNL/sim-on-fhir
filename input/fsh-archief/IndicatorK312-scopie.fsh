// ============================================================
// K3.1.2 — Scopie Verrichting
// % patiënten dat een scopie krijgt
//
// NZa-codes: 034620, 034686, 034690, 035582
// Specialismen: 0318 (MDL), 0313 (IG/MDL), 0303 (Chirurgie)
// Diagnosecodes: 601, 602 (0318) | 922, 923 (0313) | 115,116,163,325,326 (0303)
// ============================================================

Profile: IBDScopie
Parent: Procedure
Id: ibd-scopie
Title: "IBD Scopie (K3.1.2)"
Description: """
  Profiel voor een colonoscopie of sigmoidoscopie bij een IBD-patiënt.
  Gebruikt voor indicator K3.1.2: percentage patiënten dat een scopie krijgt.
  
  Geldige NZa-verrichtingcodes: 034620, 034686, 034690, 035582.
  Uitvoerende specialismen: 0318 (MDL), 0313 (Interne Geneeskunde/MDL), 0303 (Chirurgie).
"""

* status 1..1 MS
* status = #completed

* code 1..1 MS
* code from IBDScopieVerrichtingVS (required)
* code ^short = "NZa-verrichtingcode voor colonoscopie/sigmoidoscopie"

* subject 1..1 MS
* subject only Reference(IBDPatient)
* subject ^short = "Verwijzing naar de IBD-patiënt (Identificatienummer)"

* performed[x] 1..1 MS
* performed[x] only dateTime
* performedDateTime ^short = "VerrichtingStartDatum"

// Uitvoerend specialisme
* performer 0..* MS
* performer.actor only Reference(Practitioner or PractitionerRole)
* performer ^short = "Uitvoerder — specialisme 0318, 0313 of 0303"

// Koppeling met Encounter (Contact SS/IMP)
* encounter MS
* encounter only Reference(IBDContact)
* encounter ^short = "Bijbehorende opname of poliklinisch contact"

// Koppeling met de IBD-diagnose (subtraject)
* reasonReference MS
* reasonReference only Reference(IBDCondition)

// ============================================================
// K3.1.2 Encounter (Contact) Profiel
// ContactTypeCode: SS (poliklinisch) of IMP (klinisch)
// ============================================================

Profile: IBDContact
Parent: Encounter
Id: ibd-contact
Title: "IBD Contact (Encounter)"
Description: """
  Profiel voor een klinisch contact (opname of poliklinisch bezoek) 
  in het kader van IBD-zorg. ContactTypeCode SS of IMP.
  Optioneel: EncounterPriorityCodingCode R (regulier) of EM (spoed).
"""

* status 1..1 MS
* class 1..1 MS
* class from IBDContactTypeVS (required)
* class ^short = "ContactTypeCode: SS (poliklinisch) of IMP (klinisch)"

* subject 1..1 MS
* subject only Reference(IBDPatient)

* period MS
* period.start MS
* period.start ^short = "BeginDatumTijd contact"
* period.end MS
* period.end ^short = "EindDatumTijd contact"

* priority MS
* priority from IBDContactPriorityVS (extensible)
* priority ^short = "EncounterPriorityCodingCode: R of EM"

* episodeOfCare MS
* episodeOfCare only Reference(IBDEpisodeOfCare)

// ============================================================
// IBD Condition Profiel (diagnose binnen subtraject)
// ============================================================

Profile: IBDCondition
Parent: Condition
Id: ibd-condition
Title: "IBD Diagnose (Condition)"
Description: """
  Profiel voor de IBD-diagnose gekoppeld aan een DBC-subtraject.
  DiagnoseCode afkomstig uit de DBC-systematiek (601, 602, 922, 923, etc.).
"""

* clinicalStatus 1..1 MS
* code 1..1 MS
* code from IBDDiagnoseVS (required)
* code ^short = "DBC-diagnosecode (601/602/922/923 etc.)"

* subject 1..1 MS
* subject only Reference(IBDPatient)

* encounter MS
* encounter only Reference(IBDContact)

// ============================================================
// ValueSets K3.1.2
// ============================================================

ValueSet: IBDScopieVerrichtingVS
Id: ibd-scopie-verrichting-vs
Title: "IBD Scopie NZa-verrichtingcodes"
Description: "NZa-verrichtingcodes voor colonoscopie en sigmoidoscopie bij IBD"
* ^status = #active
* https://declaratie.nza.nl/verrichting#034620 "Colonoscopie diagnostisch"
* https://declaratie.nza.nl/verrichting#034686 "Colonoscopie therapeutisch"
* https://declaratie.nza.nl/verrichting#034690 "Sigmoidoscopie"
* https://declaratie.nza.nl/verrichting#035582 "Scopie overig IBD"

ValueSet: IBDContactTypeVS
Id: ibd-contact-type-vs
Title: "IBD Contact Type ValueSet"
Description: "Toegestane contacttypes voor IBD-zorgcontacten (SS=poliklinisch, IMP=klinisch)"
* ^status = #active
* http://terminology.hl7.org/CodeSystem/v3-ActCode#AMB "SS — Poliklinisch"
* http://terminology.hl7.org/CodeSystem/v3-ActCode#IMP "IMP — Klinische opname"

ValueSet: IBDContactPriorityVS
Id: ibd-contact-priority-vs
Title: "IBD Contact Prioriteit ValueSet"
Description: "Prioriteit van het contact: R (regulier) of EM (spoed)"
* ^status = #active
* http://terminology.hl7.org/CodeSystem/v3-ActPriority#R  "Regulier"
* http://terminology.hl7.org/CodeSystem/v3-ActPriority#EM "Spoed"
