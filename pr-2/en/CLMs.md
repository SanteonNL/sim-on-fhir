# SIM Logical Models - SIM on FHIR v0.1.0

## SIM Logical Models

The Santeon Information Model (SIM) is the content authority of this guide. Each logical model describes a selection of SIM data elements and their meaning, independent of any exchange format. Its mappings record where the data comes from, how it compares semantically with the Nictiz GBB-afsprakenmodel, and how it is translated into a Santeon FHIR profile. The resulting FHIR representation is described on [FHIR Profiles](profiles.md).

### Overview

10 logical models: 1 ready for validation, 9 in progress.

| | | | |
| :--- | :--- | :--- | :--- |
| [Algemene meting (SIMAlgemeneMeting)](StructureDefinition-SIMAlgemeneMeting.md) | [Observation (SAN)](StructureDefinition-observation-san.md) | [SIM CSV](StructureDefinition-SIMAlgemeneMeting-mappings.md)·[Santeon FHIR](StructureDefinition-SIMAlgemeneMeting-mappings.md) | In progress · 5 open checks |
| [Contact (SIMContact)](StructureDefinition-SIMContact.md) | [Encounter (SAN)](StructureDefinition-encounter-san.md) | [SIM CSV](StructureDefinition-SIMContact-mappings.md)·[Santeon FHIR](StructureDefinition-SIMContact-mappings.md) | In progress · 8 open checks |
| [DBC/Zorgtraject (SIMDBC)](StructureDefinition-SIMDBC.md) | [EpisodeOfCare (SAN)](StructureDefinition-episodeOfCare-san.md) | [SIM CSV](StructureDefinition-SIMDBC-mappings.md)·[Santeon FHIR](StructureDefinition-SIMDBC-mappings.md) | In progress · 6 open checks |
| [Fasering (SIMFasering)](StructureDefinition-SIMFasering.md) | – | [SIM CSV](StructureDefinition-SIMFasering-mappings.md) | In progress · 3 open checks |
| [Medicatieafspraak (SIMMedicatieAfspraak)](StructureDefinition-SIMMedicatieAfspraak.md) | [MedicationRequest (SAN)](StructureDefinition-medicationRequest-san.md) | [SIM CSV](StructureDefinition-SIMMedicatieAfspraak-mappings.md)·[Santeon FHIR](StructureDefinition-SIMMedicatieAfspraak-mappings.md) | In progress · 5 open checks |
| [Patiënt (SIMPatient)](StructureDefinition-SIMPatient.md) | [Patient (SAN)](StructureDefinition-patient-san.md) | [SIM CSV](StructureDefinition-SIMPatient-mappings.md)·[Nictiz GBB](StructureDefinition-SIMPatient-mappings.md)·[Santeon FHIR](StructureDefinition-SIMPatient-mappings.md) | Ready for validation · 2 open checks |
| [Tabakgebruik (SIMTabakGebruik)](StructureDefinition-SIMTabakGebruik.md) | [Observation TabakGebruik (SAN)](StructureDefinition-observation-san-tabakgebruik.md) | [SIM CSV](StructureDefinition-SIMTabakGebruik-mappings.md)·[Santeon FHIR](StructureDefinition-SIMTabakGebruik-mappings.md) | In progress · 2 open checks |
| [Verrichting (SIMVerrichting)](StructureDefinition-SIMVerrichting.md) | [Procedure (SAN)](StructureDefinition-procedure-san.md) | [SIM CSV](StructureDefinition-SIMVerrichting-mappings.md)·[Nictiz GBB](StructureDefinition-SIMVerrichting-mappings.md)·[Santeon FHIR](StructureDefinition-SIMVerrichting-mappings.md) | In progress · 5 open checks |
| [Vragenlijstantwoord (SIMVragenlijstAntwoord)](StructureDefinition-SIMVragenlijstAntwoord.md) | [QuestionnaireResponse (SAN)](StructureDefinition-questionnaireResponse-san.md) | [SIM CSV](StructureDefinition-SIMVragenlijstAntwoord-mappings.md)·[Santeon FHIR](StructureDefinition-SIMVragenlijstAntwoord-mappings.md) | In progress · 5 open checks |
| [Zorgplan (SIMCarePlan)](StructureDefinition-SIMCarePlan.md) | [CarePlan (SAN)](StructureDefinition-carePlan-san.md) | [SIM CSV](StructureDefinition-SIMCarePlan-mappings.md)·[Santeon FHIR](StructureDefinition-SIMCarePlan-mappings.md) | In progress · 3 open checks |

### Reading the table

**Mappings** are listed in their fixed order and only where they exist; each links to the mapping page of the logical model.

* **SIM CSV** – provenance: the original SIM CSV attributes.
* **Nictiz GBB** – semantic delta analysis with the Nictiz GBB-afsprakenmodel.
* **Santeon FHIR** – technical translation, harmonisation and required conversions to the Santeon FHIR profile, including the delivery and conversion agreements. The generator also writes it into the profile as mapping 1. The **FHIR profile** column is derived from this mapping.

**Status** is the own status of each logical model, chosen by review and recorded at the bottom of its FSH source. It is never taken over from a linked model or profile.

* **Not reviewed** – present, but not yet checked against the modelling convention (default).
* **In progress** – the definition and/or its mappings are being worked out or revised.
* **Ready for validation** – modelling and documentation are worked out; checks or technical validation may still be open.
* **Validated** – the agreed content reviews and technical validations have been completed and checked.
* **Open checks** – the number of `// TODO:` items in the FSH source of this definition.

The modelling convention (the fixed mapping order, delta indicators and consistency checks) is described in `docs/modelleerworkflow.md` in the project repository. Delta outcomes are recorded per element: 🟢/🟡 for Nictiz GBB and Equivalent/🟡 for Nictiz nl-core. The modelling status is recorded separately.

