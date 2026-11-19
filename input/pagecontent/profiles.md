<!-- manual-profiles:start -->

The Santeon FHIR profiles describe the standardised FHIR R4 representation of the SIM building blocks: the datatypes, cardinalities, terminology bindings and constraints that a resource must meet. The meaning of the data is defined by the corresponding [SIM Logical Model](CLMs.html); the Nictiz nl-core mapping records the technical comparison with the Nictiz nl-core profiles.

### Overview

10 profiles: 1 ready for validation, 9 not reviewed.

| FHIR profile | Logical Model | Mappings | Status |
|---|---|---|---|
| [CarePlan (SAN)](StructureDefinition-carePlan-san.html) | [Zorgplan (SIMCarePlan)](StructureDefinition-SIMCarePlan.html) | [Santeon FHIR](StructureDefinition-carePlan-san-mappings.html) · [Bron](StructureDefinition-carePlan-san-mappings.html) | Not reviewed · 2 open checks |
| [Condition (SAN)](StructureDefinition-condition-san.html) | – | [Bron](StructureDefinition-condition-san-mappings.html) | Not reviewed · 1 open check |
| [Encounter (SAN)](StructureDefinition-encounter-san.html) | [Contact (SIMContact)](StructureDefinition-SIMContact.html) | [Santeon FHIR](StructureDefinition-encounter-san-mappings.html) · [Bron](StructureDefinition-encounter-san-mappings.html) | Not reviewed · 2 open checks |
| [EpisodeOfCare (SAN)](StructureDefinition-episodeOfCare-san.html) | [DBC/Zorgtraject (SIMDBC)](StructureDefinition-SIMDBC.html) | [Santeon FHIR](StructureDefinition-episodeOfCare-san-mappings.html) · [Bron](StructureDefinition-episodeOfCare-san-mappings.html) | Not reviewed · 2 open checks |
| [MedicationRequest (SAN)](StructureDefinition-medicationRequest-san.html) | [Medicatieafspraak (SIMMedicatieAfspraak)](StructureDefinition-SIMMedicatieAfspraak.html) | [Santeon FHIR](StructureDefinition-medicationRequest-san-mappings.html) · [Bron](StructureDefinition-medicationRequest-san-mappings.html) | Not reviewed · 2 open checks |
| [Observation (SAN)](StructureDefinition-observation-san.html) | [Algemene meting (SIMAlgemeneMeting)](StructureDefinition-SIMAlgemeneMeting.html) | [Santeon FHIR](StructureDefinition-observation-san-mappings.html) · [Bron](StructureDefinition-observation-san-mappings.html) | Not reviewed · 1 open check |
| [Observation TabakGebruik (SAN)](StructureDefinition-observation-san-tabakgebruik.html) | [Tabakgebruik (SIMTabakGebruik)](StructureDefinition-SIMTabakGebruik.html) | [Santeon FHIR](StructureDefinition-observation-san-tabakgebruik-mappings.html) · [Bron](StructureDefinition-observation-san-tabakgebruik-mappings.html) | Not reviewed |
| [Patient (SAN)](StructureDefinition-patient-san.html) | [Patiënt (SIMPatient)](StructureDefinition-SIMPatient.html) | [Santeon FHIR](StructureDefinition-patient-san-mappings.html) · [Nictiz nl-core](StructureDefinition-patient-san-mappings.html) | Ready for validation · 2 open checks |
| [Procedure (SAN)](StructureDefinition-procedure-san.html) | [Verrichting (SIMVerrichting)](StructureDefinition-SIMVerrichting.html) | [Santeon FHIR](StructureDefinition-procedure-san-mappings.html) · [Nictiz nl-core](StructureDefinition-procedure-san-mappings.html) · [Bron](StructureDefinition-procedure-san-mappings.html) | Not reviewed |
| [QuestionnaireResponse (SAN)](StructureDefinition-questionnaireResponse-san.html) | [Vragenlijstantwoord (SIMVragenlijstAntwoord)](StructureDefinition-SIMVragenlijstAntwoord.html) | [Santeon FHIR](StructureDefinition-questionnaireResponse-san-mappings.html) · [Bron](StructureDefinition-questionnaireResponse-san-mappings.html) | Not reviewed · 1 open check |

### Reading the table

**Mappings** are listed only where they exist; each links to the mapping page of the profile.

- **Santeon FHIR** – the translation from the logical model. It is maintained with the logical model only and shown on the profile's mapping page under *Mappings to Structures in this Implementation Guide*. The *Logical Model* column is derived from this mapping.
- **Nictiz nl-core** – technical delta analysis with the Nictiz nl-core profile.
- **Bron** – a direct mapping from the SIM source attributes that predates the logical-model convention.

**Status** is the own status of each FHIR profile, chosen by review and recorded at the bottom of its FSH source. It is never taken over from a linked model or profile.

- **Not reviewed** – present, but not yet checked against the modelling convention (default).
- **In progress** – the definition and/or its mappings are being worked out or revised.
- **Ready for validation** – modelling and documentation are worked out; checks or technical validation may still be open.
- **Validated** – the agreed content reviews and technical validations have been completed and checked.
- *Open checks* – the number of `// TODO:` items in the FSH source of this definition.

The modelling convention (the fixed mapping order, delta indicators and consistency checks) is described in `docs/modelleerworkflow.md` in the project repository. The 🟢/🟡 indicators are used only within the delta-analysis mappings, not as a status of a building block.

<!-- manual-profiles:end -->
