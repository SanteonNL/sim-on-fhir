<!-- manual-sim:start -->

The Santeon Information Model (SIM) is the content authority of this guide. Each logical model describes a selection of SIM data elements and their meaning, independent of any exchange format. Its mappings record where the data comes from, how it compares semantically with the Nictiz GBB-afsprakenmodel, and how it is translated into a Santeon FHIR profile. The resulting FHIR representation is described on [FHIR Profiles](profiles.html).

### Overview

10 logical models: 1 ready for validation, 1 in progress, 8 not reviewed.

| Logical Model | FHIR profile | Mappings | Status |
|---|---|---|---|
| [Algemene meting (SIMAlgemeneMeting)](StructureDefinition-SIMAlgemeneMeting.html) | [Observation (SAN)](StructureDefinition-observation-san.html) | [Bron](StructureDefinition-SIMAlgemeneMeting-mappings.html) · [Santeon FHIR](StructureDefinition-SIMAlgemeneMeting-mappings.html) | Not reviewed |
| [Contact (SIMContact)](StructureDefinition-SIMContact.html) | [Encounter (SAN)](StructureDefinition-encounter-san.html) | [Bron](StructureDefinition-SIMContact-mappings.html) · [Santeon FHIR](StructureDefinition-SIMContact-mappings.html) | Not reviewed |
| [DBC/Zorgtraject (SIMDBC)](StructureDefinition-SIMDBC.html) | [EpisodeOfCare (SAN)](StructureDefinition-episodeOfCare-san.html) | [Bron](StructureDefinition-SIMDBC-mappings.html) · [Santeon FHIR](StructureDefinition-SIMDBC-mappings.html) | Not reviewed · 1 open check |
| [Fasering (SIMFasering)](StructureDefinition-SIMFasering.html) | – | [Bron](StructureDefinition-SIMFasering-mappings.html) | Not reviewed |
| [Medicatieafspraak (SIMMedicatieAfspraak)](StructureDefinition-SIMMedicatieAfspraak.html) | [MedicationRequest (SAN)](StructureDefinition-medicationRequest-san.html) | [Bron](StructureDefinition-SIMMedicatieAfspraak-mappings.html) · [Santeon FHIR](StructureDefinition-SIMMedicatieAfspraak-mappings.html) | Not reviewed |
| [Patiënt (SIMPatient)](StructureDefinition-SIMPatient.html) | [Patient (SAN)](StructureDefinition-patient-san.html) | [Bron](StructureDefinition-SIMPatient-mappings.html) · [Nictiz GBB](StructureDefinition-SIMPatient-mappings.html) · [Santeon FHIR](StructureDefinition-SIMPatient-mappings.html) | Ready for validation |
| [Tabakgebruik (SIMTabakGebruik)](StructureDefinition-SIMTabakGebruik.html) | [Observation TabakGebruik (SAN)](StructureDefinition-observation-san-tabakgebruik.html) | [Bron](StructureDefinition-SIMTabakGebruik-mappings.html) · [Santeon FHIR](StructureDefinition-SIMTabakGebruik-mappings.html) | Not reviewed |
| [Verrichting (SIMVerrichting)](StructureDefinition-SIMVerrichting.html) | [Procedure (SAN)](StructureDefinition-procedure-san.html) | [Bron](StructureDefinition-SIMVerrichting-mappings.html) · [Nictiz GBB](StructureDefinition-SIMVerrichting-mappings.html) · [Santeon FHIR](StructureDefinition-SIMVerrichting-mappings.html) | In progress |
| [Vragenlijstantwoord (SIMVragenlijstAntwoord)](StructureDefinition-SIMVragenlijstAntwoord.html) | [QuestionnaireResponse (SAN)](StructureDefinition-questionnaireResponse-san.html) | [Bron](StructureDefinition-SIMVragenlijstAntwoord-mappings.html) · [Santeon FHIR](StructureDefinition-SIMVragenlijstAntwoord-mappings.html) | Not reviewed |
| [Zorgplan (SIMCarePlan)](StructureDefinition-SIMCarePlan.html) | [CarePlan (SAN)](StructureDefinition-carePlan-san.html) | [Bron](StructureDefinition-SIMCarePlan-mappings.html) · [Santeon FHIR](StructureDefinition-SIMCarePlan-mappings.html) | Not reviewed |

### Reading the table

**Mappings** are listed in their fixed order and only where they exist; each links to the mapping page of the logical model.

- **Bron** – provenance: the original SIM source attributes.
- **Nictiz GBB** – semantic delta analysis with the Nictiz GBB-afsprakenmodel.
- **Santeon FHIR** – technical translation, harmonisation and required conversions to the Santeon FHIR profile. The *FHIR profile* column is derived from this mapping.

**Status** is the own status of each logical model, chosen by review and recorded at the bottom of its FSH source. It is never taken over from a linked model or profile.

- **Not reviewed** – present, but not yet checked against the modelling convention (default).
- **In progress** – the definition and/or its mappings are being worked out or revised.
- **Ready for validation** – modelling and documentation are worked out; checks or technical validation may still be open.
- **Validated** – the agreed content reviews and technical validations have been completed and checked.
- *Open checks* – the number of `// TODO:` items in the FSH source of this definition.

The modelling convention (the fixed mapping order, delta indicators and consistency checks) is described in `docs/modelleerworkflow.md` in the project repository. The 🟢/🟡 indicators are used only within the delta-analysis mappings, not as a status of a building block.

<!-- manual-sim:end -->
