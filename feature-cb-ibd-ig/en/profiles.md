# FHIR Profiles - SIM on FHIR v0.1.0

## FHIR Profiles

The Santeon FHIR profiles describe the standardised FHIR R4 representation of the SIM building blocks: the datatypes, cardinalities, terminology bindings and constraints that a resource must meet. The meaning of the data is defined by the corresponding [SIM Logical Model](CLMs.md); the Nictiz nl-core mapping records the technical comparison with the Nictiz nl-core profiles.

### Overview

10 profiles: 1 ready for validation, 9 not reviewed.

| | | | |
| :--- | :--- | :--- | :--- |
| [CarePlan (SAN)](StructureDefinition-carePlan-san.md) | [Zorgplan (SIMCarePlan)](StructureDefinition-SIMCarePlan.md) | [Santeon FHIR](StructureDefinition-carePlan-san-mappings.md)·[Bron](StructureDefinition-carePlan-san-mappings.md) | Not reviewed · 2 open checks |
| [Condition (SAN)](StructureDefinition-condition-san.md) | – | [Bron](StructureDefinition-condition-san-mappings.md) | Not reviewed · 1 open check |
| [Encounter (SAN)](StructureDefinition-encounter-san.md) | [Contact (SIMContact)](StructureDefinition-SIMContact.md) | [Santeon FHIR](StructureDefinition-encounter-san-mappings.md)·[Bron](StructureDefinition-encounter-san-mappings.md) | Not reviewed · 2 open checks |
| [EpisodeOfCare (SAN)](StructureDefinition-episodeOfCare-san.md) | [DBC/Zorgtraject (SIMDBC)](StructureDefinition-SIMDBC.md) | [Santeon FHIR](StructureDefinition-episodeOfCare-san-mappings.md)·[Bron](StructureDefinition-episodeOfCare-san-mappings.md) | Not reviewed · 2 open checks |
| [MedicationRequest (SAN)](StructureDefinition-medicationRequest-san.md) | [Medicatieafspraak (SIMMedicatieAfspraak)](StructureDefinition-SIMMedicatieAfspraak.md) | [Santeon FHIR](StructureDefinition-medicationRequest-san-mappings.md)·[Bron](StructureDefinition-medicationRequest-san-mappings.md) | Not reviewed · 2 open checks |
| [Observation (SAN)](StructureDefinition-observation-san.md) | [Algemene meting (SIMAlgemeneMeting)](StructureDefinition-SIMAlgemeneMeting.md) | [Santeon FHIR](StructureDefinition-observation-san-mappings.md)·[Bron](StructureDefinition-observation-san-mappings.md) | Not reviewed · 1 open check |
| [Observation TabakGebruik (SAN)](StructureDefinition-observation-san-tabakgebruik.md) | [Tabakgebruik (SIMTabakGebruik)](StructureDefinition-SIMTabakGebruik.md) | [Santeon FHIR](StructureDefinition-observation-san-tabakgebruik-mappings.md)·[Bron](StructureDefinition-observation-san-tabakgebruik-mappings.md) | Not reviewed |
| [Patient (SAN)](StructureDefinition-patient-san.md) | [Patiënt (SIMPatient)](StructureDefinition-SIMPatient.md) | [Santeon FHIR](StructureDefinition-patient-san-mappings.md)·[Nictiz nl-core](StructureDefinition-patient-san-mappings.md) | Ready for validation · 2 open checks |
| [Procedure (SAN)](StructureDefinition-procedure-san.md) | [Verrichting (SIMVerrichting)](StructureDefinition-SIMVerrichting.md) | [Santeon FHIR](StructureDefinition-procedure-san-mappings.md)·[Nictiz nl-core](StructureDefinition-procedure-san-mappings.md)·[Bron](StructureDefinition-procedure-san-mappings.md) | Not reviewed |
| [QuestionnaireResponse (SAN)](StructureDefinition-questionnaireResponse-san.md) | [Vragenlijstantwoord (SIMVragenlijstAntwoord)](StructureDefinition-SIMVragenlijstAntwoord.md) | [Santeon FHIR](StructureDefinition-questionnaireResponse-san-mappings.md)·[Bron](StructureDefinition-questionnaireResponse-san-mappings.md) | Not reviewed · 1 open check |

### Reading the table

**Mappings** are listed only where they exist; each links to the mapping page of the profile.

* **Santeon FHIR** – the translation from the logical model. It is maintained with the logical model only and shown on the profile's mapping page under **Mappings to Structures in this Implementation Guide**. The **Logical Model** column is derived from this mapping.
* **Nictiz nl-core** – technical delta analysis with the Nictiz nl-core profile.
* **Bron** – a direct mapping from the SIM source attributes that predates the logical-model convention.

**Status** is the own status of each FHIR profile, chosen by review and recorded at the bottom of its FSH source. It is never taken over from a linked model or profile.

* **Not reviewed** – present, but not yet checked against the modelling convention (default).
* **In progress** – the definition and/or its mappings are being worked out or revised.
* **Ready for validation** – modelling and documentation are worked out; checks or technical validation may still be open.
* **Validated** – the agreed content reviews and technical validations have been completed and checked.
* **Open checks** – the number of `// TODO:` items in the FSH source of this definition.

The modelling convention (the fixed mapping order, delta indicators and consistency checks) is described in `docs/modelleerworkflow.md` in the project repository. The 🟢/🟡 indicators are used only within the delta-analysis mappings, not as a status of a building block.

