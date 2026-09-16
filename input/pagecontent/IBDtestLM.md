Onderstaande structuur beschrijft de selectie van informatie voor de IBD-dataset vanuit Logical Models.

### Cohortselectie
- [IBD Cohort Patient](StructureDefinition-IBDCohortPatient.html)
- [IBD Cohort DBC](StructureDefinition-IBDCohortDBC.html)

### DSD selectie per data-element (optie I)
- [IBD Dataset AlgemeneMeting](StructureDefinition-IBDAlgemeneMeting.html)
- [IBD Dataset Verrichting](StructureDefinition-IBDVerrichting.html)
- [IBD Dataset DBC](StructureDefinition-IBDZorgtrajectDBC.html)

### DSD selectie per indicator (optie II)
- [K311 WIP]()
- [K312 WIP]()
- [K363 WIP]()

### Bundles?
- Batch

<!--

| Element | Selectie | Validatie |
| :--- | :--- | :--- | :--- |
| [SIMAlgemeneMeting.`MetingDatumTijd`](StructureDefinition-SIMAlgemeneMeting-mappings.html) | >= 2018-01-01 | |
| [SIMAlgemeneMeting.`MetingNaam`](StructureDefinition-SIMAlgemeneMeting-mappings.html) | [ValueSet: IBDAlgemeneMetingen](Valueset-IBDAlgemeneMetingen.html) | |
| [SIMAlgemeneMeting.`UitslagCode`](StructureDefinition-SIMAlgemeneMeting-mappings.html) | | [ValueSet: AlcoholGebruikStatusCodelijst](Valueset-AlcoholGebruikStatusCodelijst.html) |
| [SIMVerrichting.`StartDatum`](StructureDefinition-SIMVerrichting-mappings.html) | >= 2018-01-01 | |
| [SIMVerrichting.`VerrichtingType`](StructureDefinition-SIMVerrichting-mappings.html) | [ValueSet: IBDVerrichtingenNZa](Valueset-IBDVerrichtingenNZa.html) | |
| [SIMDBC.`OpeningsDatum`](StructureDefinition-SIMDBC-mappings.html) | >= 2017-01-01 | |
| [SIMDBC.`SpecialismeDiagnose`](StructureDefinition-SIMDBC-mappings.html) | [ValueSet: IBDSpecialismeDiagnoses](Valueset-IBDSpecialismeDiagnoses.html) | |


### FHIR query
```text
GET [base]/Observation?
  code:in=Valueset/IBDAlgemeneMetingen&
  effectiveDateTime=ge2018-01-01

Etc.
``` -->
