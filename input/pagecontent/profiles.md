<!-- manual-profiles:start -->

The Santeon FHIR profiles describe the standardised FHIR R4 representation of the SIM building blocks. A profile only adds rules for agreed FHIR requirements that its parent does not cover yet. The scope and the delivery and conversion agreements are maintained in the corresponding [SIM Logical Model](CLMs.html) and its mapping to FHIR.

### Overview

10 profiles: 1 ready for validation, 5 in progress, 4 not reviewed.

| FHIR profile | Logical Model | Mappings | Status |
|---|---|---|---|
| [CarePlan (SAN)](StructureDefinition-carePlan-san.html) | [Zorgplan (SIMCarePlan)](StructureDefinition-SIMCarePlan.html) | [SIM Logical Model](StructureDefinition-carePlan-san-mappings.html) | In progress · 3 open checks |
| [Condition (SAN)](StructureDefinition-condition-san.html) | – | – | Not reviewed · 3 open checks |
| [Encounter (SAN)](StructureDefinition-encounter-san.html) | [Contact (SIMContact)](StructureDefinition-SIMContact.html) | [SIM Logical Model](StructureDefinition-encounter-san-mappings.html) | In progress · 4 open checks |
| [EpisodeOfCare (SAN)](StructureDefinition-episodeOfCare-san.html) | [DBC/Zorgtraject (SIMDBC)](StructureDefinition-SIMDBC.html) | [SIM Logical Model](StructureDefinition-episodeOfCare-san-mappings.html) | In progress · 3 open checks |
| [MedicationRequest (SAN)](StructureDefinition-medicationRequest-san.html) | [Medicatieafspraak (SIMMedicatieAfspraak)](StructureDefinition-SIMMedicatieAfspraak.html) | [SIM Logical Model](StructureDefinition-medicationRequest-san-mappings.html) | In progress · 4 open checks |
| [Observation (SAN)](StructureDefinition-observation-san.html) | [Algemene meting (SIMAlgemeneMeting)](StructureDefinition-SIMAlgemeneMeting.html) | [SIM Logical Model](StructureDefinition-observation-san-mappings.html) | In progress · 2 open checks |
| [Observation TabakGebruik (SAN)](StructureDefinition-observation-san-tabakgebruik.html) | [Tabakgebruik (SIMTabakGebruik)](StructureDefinition-SIMTabakGebruik.html) | [SIM Logical Model](StructureDefinition-observation-san-tabakgebruik-mappings.html) | Not reviewed · 3 open checks |
| [Patient (SAN)](StructureDefinition-patient-san.html) | [Patiënt (SIMPatient)](StructureDefinition-SIMPatient.html) | [SIM Logical Model](StructureDefinition-patient-san-mappings.html) · [Nictiz nl-core](StructureDefinition-patient-san-mappings.html) | Ready for validation · 1 open check |
| [Procedure (SAN)](StructureDefinition-procedure-san.html) | [Verrichting (SIMVerrichting)](StructureDefinition-SIMVerrichting.html) | [SIM Logical Model](StructureDefinition-procedure-san-mappings.html) · [Nictiz nl-core](StructureDefinition-procedure-san-mappings.html) | Not reviewed · 2 open checks |
| [QuestionnaireResponse (SAN)](StructureDefinition-questionnaireResponse-san.html) | [Vragenlijstantwoord (SIMVragenlijstAntwoord)](StructureDefinition-SIMVragenlijstAntwoord.html) | [SIM Logical Model](StructureDefinition-questionnaireResponse-san-mappings.html) | Not reviewed · 3 open checks |

### Reading the table

**Mappings** are listed in their fixed order and only where they exist; each links to the mapping page of the profile.

- **SIM Logical Model** (mapping 1) – SIM provenance and conversion per FHIR element. Generated from the logical model's mapping to FHIR; maintained there only. The *Logical Model* column is derived from this mapping.
- **Nictiz nl-core** (mapping 2) – manual technical delta analysis with the Nictiz nl-core profile (nictiz.nl.gbb 1.0.0-alpha.2). Where no technical delta is recorded, the FSH source explains why.

Each conversion note starts with one label, the first that applies:

- **Afleiding** – the FHIR value is not in the source and is derived or calculated from SIM data or a fixed agreement.
- **Structuurconversie** – the structure changes: SIM elements are combined or split, an identifier becomes a reference, records are grouped, or the target path depends on a condition.
- **Terminologieconversie** – codes are translated to another code system or value set.
- **Datatypeconversie** – the value stays the same, but the datatype changes (such as date to dateTime).
- **Direct** – one SIM value goes unchanged to one FHIR element, also under another element name, a specific choice type or a nested path.

**Status** is the own status of each FHIR profile, chosen by review and recorded at the bottom of its FSH source. It is never taken over from a linked model or profile.

- **Not reviewed** – present, but not yet checked against the modelling convention (default).
- **In progress** – the definition and/or its mappings are being worked out or revised.
- **Ready for validation** – modelling and documentation are worked out; checks or technical validation may still be open.
- **Validated** – the agreed content reviews and technical validations have been completed and checked.
- *Open checks* – the number of `// TODO:` items in the FSH source of this definition.

The modelling convention (the fixed mapping order, delta indicators and consistency checks) is described in `docs/modelleerworkflow.md` in the project repository. Delta outcomes against Nictiz GBB and Nictiz nl-core are recorded per element: Equivalent, or 🟡 for a difference or an open assessment. The modelling status is recorded separately.

<!-- manual-profiles:end -->
