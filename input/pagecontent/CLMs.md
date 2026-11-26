<!-- manual-sim:start -->

The Santeon Information Model (SIM) is the content authority of this guide. Each logical model describes a selection of SIM data elements and their meaning, independent of any exchange format. Its mappings record where the data comes from, how it compares semantically with the Nictiz GBB-afsprakenmodel, and how it is translated into a Santeon FHIR profile. The resulting FHIR representation is described on [FHIR Profiles](profiles.html).

### Overview

10 logical models: 1 ready for validation, 9 in progress.

| Logical Model | FHIR profile | Mappings | Status |
|---|---|---|---|
| [Algemene meting (SIMAlgemeneMeting)](StructureDefinition-SIMAlgemeneMeting.html) | [Observation (SAN)](StructureDefinition-observation-san.html) | [SIM CSV](StructureDefinition-SIMAlgemeneMeting-mappings.html) · [Santeon FHIR](StructureDefinition-SIMAlgemeneMeting-mappings.html) | In progress · 5 open checks |
| [Contact (SIMContact)](StructureDefinition-SIMContact.html) | [Encounter (SAN)](StructureDefinition-encounter-san.html) | [SIM CSV](StructureDefinition-SIMContact-mappings.html) · [Santeon FHIR](StructureDefinition-SIMContact-mappings.html) | In progress · 8 open checks |
| [DBC/Zorgtraject (SIMDBC)](StructureDefinition-SIMDBC.html) | [EpisodeOfCare (SAN)](StructureDefinition-episodeOfCare-san.html) | [SIM CSV](StructureDefinition-SIMDBC-mappings.html) · [Santeon FHIR](StructureDefinition-SIMDBC-mappings.html) | In progress · 6 open checks |
| [Fasering (SIMFasering)](StructureDefinition-SIMFasering.html) | – | [SIM CSV](StructureDefinition-SIMFasering-mappings.html) | In progress · 3 open checks |
| [Medicatieafspraak (SIMMedicatieAfspraak)](StructureDefinition-SIMMedicatieAfspraak.html) | [MedicationRequest (SAN)](StructureDefinition-medicationRequest-san.html) | [SIM CSV](StructureDefinition-SIMMedicatieAfspraak-mappings.html) · [Santeon FHIR](StructureDefinition-SIMMedicatieAfspraak-mappings.html) | In progress · 5 open checks |
| [Patiënt (SIMPatient)](StructureDefinition-SIMPatient.html) | [Patient (SAN)](StructureDefinition-patient-san.html) | [SIM CSV](StructureDefinition-SIMPatient-mappings.html) · [Nictiz GBB](StructureDefinition-SIMPatient-mappings.html) · [Santeon FHIR](StructureDefinition-SIMPatient-mappings.html) | Ready for validation · 2 open checks |
| [Tabakgebruik (SIMTabakGebruik)](StructureDefinition-SIMTabakGebruik.html) | [Observation TabakGebruik (SAN)](StructureDefinition-observation-san-tabakgebruik.html) | [SIM CSV](StructureDefinition-SIMTabakGebruik-mappings.html) · [Santeon FHIR](StructureDefinition-SIMTabakGebruik-mappings.html) | In progress · 2 open checks |
| [Verrichting (SIMVerrichting)](StructureDefinition-SIMVerrichting.html) | [Procedure (SAN)](StructureDefinition-procedure-san.html) | [SIM CSV](StructureDefinition-SIMVerrichting-mappings.html) · [Nictiz GBB](StructureDefinition-SIMVerrichting-mappings.html) · [Santeon FHIR](StructureDefinition-SIMVerrichting-mappings.html) | In progress · 6 open checks |
| [Vragenlijstantwoord (SIMVragenlijstAntwoord)](StructureDefinition-SIMVragenlijstAntwoord.html) | [QuestionnaireResponse (SAN)](StructureDefinition-questionnaireResponse-san.html) | [SIM CSV](StructureDefinition-SIMVragenlijstAntwoord-mappings.html) · [Santeon FHIR](StructureDefinition-SIMVragenlijstAntwoord-mappings.html) | In progress · 5 open checks |
| [Zorgplan (SIMCarePlan)](StructureDefinition-SIMCarePlan.html) | [CarePlan (SAN)](StructureDefinition-carePlan-san.html) | [SIM CSV](StructureDefinition-SIMCarePlan-mappings.html) · [Santeon FHIR](StructureDefinition-SIMCarePlan-mappings.html) | In progress · 3 open checks |

### Reading the table

**Mappings** are listed in their fixed order and only where they exist; each links to the mapping page of the logical model.

- **SIM CSV** – provenance: the original SIM CSV attributes.
- **Nictiz GBB** – semantic delta analysis with the Nictiz GBB-afsprakenmodel.
- **Santeon FHIR** – technical translation, harmonisation and required conversions to the Santeon FHIR profile, including the delivery and conversion agreements. The generator also writes it into the profile as mapping 1. The *FHIR profile* column is derived from this mapping.

Each conversion note starts with one label, the first that applies:

- **Afleiding** – the FHIR value is not in the source and is derived or calculated from SIM data or a fixed agreement.
- **Structuurconversie** – the structure changes: SIM elements are combined or split, an identifier becomes a reference, records are grouped, or the target path depends on a condition.
- **Terminologieconversie** – codes are translated to another code system or value set.
- **Datatypeconversie** – the value stays the same, but the datatype changes (such as date to dateTime).
- **Direct** – one SIM value goes unchanged to one FHIR element, also under another element name, a specific choice type or a nested path.

**Status** is the own status of each logical model, chosen by review and recorded at the bottom of its FSH source. It is never taken over from a linked model or profile.

- **Not reviewed** – present, but not yet checked against the modelling convention (default).
- **In progress** – the definition and/or its mappings are being worked out or revised.
- **Ready for validation** – modelling and documentation are worked out; checks or technical validation may still be open.
- **Validated** – the agreed content reviews and technical validations have been completed and checked.
- *Open checks* – the number of `// TODO:` items in the FSH source of this definition.

The modelling convention (the fixed mapping order, delta indicators and consistency checks) is described in `docs/modelleerworkflow.md` in the project repository. Delta outcomes against Nictiz GBB and Nictiz nl-core are recorded per element: Equivalent, or 🟡 for a difference or an open assessment. The modelling status is recorded separately.

<!-- manual-sim:end -->
