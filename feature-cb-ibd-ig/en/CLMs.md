# SIM Logical Models - SIM on FHIR v0.1.0

## SIM Logical Models

The Santeon Information Model (SIM) is the content authority of this guide. Each logical model describes a selection of SIM data elements and their meaning, independent of any exchange format. Its mappings record where the data comes from, how it compares semantically with the Nictiz GBB-afsprakenmodel, and how it is translated into a Santeon FHIR profile. The resulting FHIR representation is described on [FHIR Profiles](profiles.md).

### Overview

10 logical models: 1 ready for validation, 1 in progress, 8 not reviewed.

| | | | |
| :--- | :--- | :--- | :--- |
| [Algemene meting (SIMAlgemeneMeting)](StructureDefinition-SIMAlgemeneMeting.md) | [Observation (SAN)](StructureDefinition-observation-san.md) | [Bron](StructureDefinition-SIMAlgemeneMeting-mappings.md)·[Santeon FHIR](StructureDefinition-SIMAlgemeneMeting-mappings.md) | Not reviewed |
| [Contact (SIMContact)](StructureDefinition-SIMContact.md) | [Encounter (SAN)](StructureDefinition-encounter-san.md) | [Bron](StructureDefinition-SIMContact-mappings.md)·[Santeon FHIR](StructureDefinition-SIMContact-mappings.md) | Not reviewed |
| [DBC/Zorgtraject (SIMDBC)](StructureDefinition-SIMDBC.md) | [EpisodeOfCare (SAN)](StructureDefinition-episodeOfCare-san.md) | [Bron](StructureDefinition-SIMDBC-mappings.md)·[Santeon FHIR](StructureDefinition-SIMDBC-mappings.md) | Not reviewed · 1 open check |
| [Fasering (SIMFasering)](StructureDefinition-SIMFasering.md) | – | [Bron](StructureDefinition-SIMFasering-mappings.md) | Not reviewed |
| [Medicatieafspraak (SIMMedicatieAfspraak)](StructureDefinition-SIMMedicatieAfspraak.md) | [MedicationRequest (SAN)](StructureDefinition-medicationRequest-san.md) | [Bron](StructureDefinition-SIMMedicatieAfspraak-mappings.md)·[Santeon FHIR](StructureDefinition-SIMMedicatieAfspraak-mappings.md) | Not reviewed |
| [Patiënt (SIMPatient)](StructureDefinition-SIMPatient.md) | [Patient (SAN)](StructureDefinition-patient-san.md) | [Bron](StructureDefinition-SIMPatient-mappings.md)·[Nictiz GBB](StructureDefinition-SIMPatient-mappings.md)·[Santeon FHIR](StructureDefinition-SIMPatient-mappings.md) | Ready for validation |
| [Tabakgebruik (SIMTabakGebruik)](StructureDefinition-SIMTabakGebruik.md) | [Observation TabakGebruik (SAN)](StructureDefinition-observation-san-tabakgebruik.md) | [Bron](StructureDefinition-SIMTabakGebruik-mappings.md)·[Santeon FHIR](StructureDefinition-SIMTabakGebruik-mappings.md) | Not reviewed |
| [Verrichting (SIMVerrichting)](StructureDefinition-SIMVerrichting.md) | [Procedure (SAN)](StructureDefinition-procedure-san.md) | [Bron](StructureDefinition-SIMVerrichting-mappings.md)·[Nictiz GBB](StructureDefinition-SIMVerrichting-mappings.md)·[Santeon FHIR](StructureDefinition-SIMVerrichting-mappings.md) | In progress |
| [Vragenlijstantwoord (SIMVragenlijstAntwoord)](StructureDefinition-SIMVragenlijstAntwoord.md) | [QuestionnaireResponse (SAN)](StructureDefinition-questionnaireResponse-san.md) | [Bron](StructureDefinition-SIMVragenlijstAntwoord-mappings.md)·[Santeon FHIR](StructureDefinition-SIMVragenlijstAntwoord-mappings.md) | Not reviewed |
| [Zorgplan (SIMCarePlan)](StructureDefinition-SIMCarePlan.md) | [CarePlan (SAN)](StructureDefinition-carePlan-san.md) | [Bron](StructureDefinition-SIMCarePlan-mappings.md)·[Santeon FHIR](StructureDefinition-SIMCarePlan-mappings.md) | Not reviewed |

### Reading the table

**Mappings** are listed in their fixed order and only where they exist; each links to the mapping page of the logical model.

* **Bron** – provenance: the original SIM source attributes.
* **Nictiz GBB** – semantic delta analysis with the Nictiz GBB-afsprakenmodel.
* **Santeon FHIR** – technical translation, harmonisation and required conversions to the Santeon FHIR profile. The **FHIR profile** column is derived from this mapping.

**Status** is the own status of each logical model, chosen by review and recorded at the bottom of its FSH source. It is never taken over from a linked model or profile.

* **Not reviewed** – present, but not yet checked against the modelling convention (default).
* **In progress** – the definition and/or its mappings are being worked out or revised.
* **Ready for validation** – modelling and documentation are worked out; checks or technical validation may still be open.
* **Validated** – the agreed content reviews and technical validations have been completed and checked.
* **Open checks** – the number of `// TODO:` items in the FSH source of this definition.

The modelling convention (the fixed mapping order, delta indicators and consistency checks) is described in `docs/modelleerworkflow.md` in the project repository. The 🟢/🟡 indicators are used only within the delta-analysis mappings, not as a status of a building block.

