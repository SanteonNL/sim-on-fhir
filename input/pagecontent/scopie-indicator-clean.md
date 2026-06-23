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

| Conceptueel | FHIR profiel & element | Waarde |
| :--- | :--- | :--- |
| DBC Begindatum | [MyModelEpisodeOfCare](StructureDefinition-my-model-episode-of-care.html)`.period.start` |
| DBC Vervallen Status (0/1) | [MyModelEpisodeOfCare](StructureDefinition-my-model-episode-of-care.html)`.extension[vervallen]` |
| Hoofddiagnose Code | [MyModelEpisodeOfCare](StructureDefinition-my-model-episode-of-care.html)`.diagnosis.condition.extension[hoofddiag]` met [ValueSet: LocalSpecialismeDiagnoseCodes](ValueSet-local-specialisme-diagnose-codes.html) |
| Landelijk Zorgtype (11/21) | [MyModelEpisodeOfCare](StructureDefinition-my-model-episode-of-care.html)`.type.coding.code` met [ValueSet: LocalZorgtypeCodes](ValueSet-local-zorgtype-codes.html) |
| Specialismecode (0313/0318/0303) | [MyModelEpisodeOfCare](StructureDefinition-my-model-episode-of-care.html)`type.text` |
| Geboortedatum Patiënt | `Patient.birthDate` | => 18 jaar |


## Measure
Definitie: Kwaliteitsindicator die het aantal unieke volwassen IBD-patiënten telt met een scopie-verrichting vanaf 2018.

<!-- **Initial Population:** IBD cohort -->
**Denominator:** [IBD cohort](scopie-indicator-clean.html#ibd-cohort)

**Numerator:** IBD cohort **met scopie** ([NZa Verrichtingcode:](ValueSet-local-verrichting-codes-nza.html) `034620`, `034686`, `034690`, `035582`)

<!-- Procedure.code=nza... -->
### Gebruikte profielen Measure

| Conceptueel | FHIR profiel & element | Waarde |
| :--- | :--- | :--- |
| IBD cohort | zie [gebruikte profielen IBD cohort](scopie-indicator-clean.html#ibd-cohort) |
| NZa Verrichtingcode | [MyModelProcedure](StructureDefinition-my-model-procedure.html)`.code.coding.code` met [ValueSet: LocalVerrichtingCodesNZa](ValueSet-local-verrichting-codes-nza.html)|

## FHIR query
GET ...

<!-- ## Korte omschrijving?
check: *3* juist (inidcator)profielen (episodeofcare,procedure,patient.geboortedatum) -->

