# Data Lineage

This page documents the mapping between the Santeon CarePlan FHIR profile and the underlying SQL data sources.

## Overview

The CarePlan profile maps to two SQL data sources:
- **LUSCII CarePlan** (`luscii_careplan.sql`) - LUSCII patient management database
- **PatientJourney CarePlan** (`pj_careplan.sql`) - PatientJourney warehouse

## Mapping Tables

For detailed element-level mappings showing which database tables and columns feed each FHIR element, see the [Profile Mappings](StructureDefinition-santeon-careplan-mappings.html) page.

### Format

Mappings are displayed in the format: `table[column]`

For example:
- `luscii_patients[Patient_Number]` - Patient Number column from the luscii_patients table
- `pj_enrolments[enrolment_date]` - Enrolment Date column from the pj_enrolments table

## Data Sources

### LUSCII Database Tables
- `luscii_patients` - Patient master data
- `luscii_programs` - Program/care definitions
- `luscii_patientsprogramshistory` - Patient program enrollment history
- `luscii_usersStatusChangeReasons` - Status change event log

### PatientJourney Database Tables
- `pj_patients` - Patient reference data
- `pj_programmes` - Programme definitions
- `pj_enrolments` - Patient enrollment information
