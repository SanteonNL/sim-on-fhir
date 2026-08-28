Beschrijving ...

## IBD Cohort
Een DBC geopend op of na 01-01-2018 met zorgtype `11` of `21`, die niet is vervallen, vallend onder de volgende codes:
   * **MDL (0318)**: Diagnosecode `601`, `602`
   * **Interne Geneeskunde (0313)**: Diagnosecode `922`, `923` (indien subspecialisme MDL)
   * **Chirurgie (0303)**: Diagnosecode `115`, `116`, `163`, `325`, `326`

**Exclusiecriteria**
* Patiënten die op de peildatum (moment van aanlevering) jonger zijn dan 18 jaar (`Patient.birthDate`).

### Gebruikte profielen IBD Cohort
<!-- EpisodeOfCare.code=dbc...
Patient.birthdate=>18y 20200101 -->

| Conceptueel | FHIR profiel & element | Waarde of Terminologie |
| :--- | :--- | :--- |
| Geboortedatum | [PatientSan](StructureDefinition-PatientSan.html)`.birthDate` | ≥ 18 jaar | |
| SpecialismeDiagnoseCode | [EpisodeOfCareSan](StructureDefinition-EpisodeOfCareSan.html)`.diagnosis.condition`→ [ConditionSan](StructureDefinition-ConditionSan.html)`.code.coding.code` | [ValueSet: LocalSpecialismeDiagnoseCodes](ValueSet-local-specialisme-diagnose-codes.html) |
| SpecialismeDiagnoseSysteem | [EpisodeOfCareSan](StructureDefinition-EpisodeOfCareSan.html)`.diagnosis.condition`→ [ConditionSan](StructureDefinition-ConditionSan.html)`.code.coding.system` | Systeem behorend bij de SpecialismeDiagnoseCode |
| SpecialismeDiagnoseOmschrijving | [EpisodeOfCareSan](StructureDefinition-EpisodeOfCareSan.html)`.diagnosis.condition`→ [ConditionSan](StructureDefinition-ConditionSan.html)`.code.coding.display` | Omschrijving behorend bij de SpecialismeDiagnoseCode 
| ZorgTypeCode | [EpisodeOfCareSan](StructureDefinition-EpisodeOfCareSan.html)`.type.coding.code` | [ValueSet: LocalZorgtypeCodes](ValueSet-local-zorgtype-codes.html) |
| ZorgTypeCodeSysteem | [EpisodeOfCareSan](StructureDefinition-EpisodeOfCareSan.html)`.type.coding.system` | Systeem behorend bij de ZorgTypeCode |
| ZorgTypeCodeOmschrijving | [EpisodeOfCareSan](StructureDefinition-EpisodeOfCareSan.html)`.type.coding.display` | Omschrijving behorend bij de ZorgTypeCode |
| DBC Geldig | [EpisodeOfCareSan](StructureDefinition-EpisodeOfCareSan.html)`.status` | Active |
| DBC OpeningsDatum | [EpisodeOfCareSan](StructureDefinition-EpisodeOfCareSan.html)`.period.start` | ≥ 2018-01-01 |

### FHIR query IBD Cohort
```text
//Cohort:

GET [base]/Patient?
  birthdate=le[Peildatum-18J]

GET [base]/EpisodeOfCare?
  type:in=ValueSet/LocalZorgtypeCodes&
  status=active&
  period-start=ge2018-01-01

GET [base]/Condition?
  code:in=ValueSet/LocalSpecialismeDiagnoseCodes
```

## IBD Indicatoren
- [K311 WIP](K311.html)
- [K312 Scopie](K312.html)
- [K363 Calprotectine voor scopie](K363.html)

Voorbeeld Bundle: