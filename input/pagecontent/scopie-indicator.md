# IBD Scopie Indicator Cohort

Deze pagina beschrijft de technische en functionele specificaties voor het selecteren van het patiëntcohort ten behoeve van de IBD Scopie indicator.

## Functionele Omschrijving
De indicator identificeert unieke volwassen patiënten (>= 18 jaar) die vanaf **01-01-2018** een scopie-gerelateerde behandeling of hulptraject hebben ondergaan binnen de specialismen MDL, Interne Geneeskunde of Chirurgie.

### IBD-Cohort
Patiënten moeten voldoen aan *ten minste één* van de volgende twee criteria:
1. **Zorgplan**: Een actief of historisch `CarePlan` met de categoriecode `ZBJ_IBD` gestart op of na 01-01-2018.
2. **DBC/DOT Combinatie**: Een DBC geopend op of na 01-01-2018 met zorgtype `11` of `21`, die niet is vervallen, vallend onder de volgende codes:
   * **MDL (0318)**: Diagnosecode `601`, `602`
   * **Interne Geneeskunde (0313)**: Diagnosecode `922`, `923` (indien subspecialisme MDL)
   * **Chirurgie (0303)**: Diagnosecode `115`, `116`, `163`, `325`, `326`

### Exclusiecriteria
* Patiënten die op de peildatum (moment van aanlevering) jonger zijn dan 18 jaar (`Patient.birthDate`).


## Measure
Kwaliteitsindicator die het aantal unieke volwassen IBD-patiënten telt met een scopie-verrichting vanaf 2018. Voor meer details zie: [IBDScopieMeasure](Measure-IbdScopieCohortMeasure.html)

### Formule: 

**Initial Population:** IBD cohort

**Denominator:** IBD cohort 

**Numerator:** IBD cohort **met scopie**; de patiënt moet een gekoppelde NZa-verrichting hebben met een van de volgende codes (vanaf 01-01-2018):
* `034620`, `034686`, `034690`, `035582` [ValueSet: LocalVerrichtingCodesNZa](ValueSet-local-verrichting-codes-nza.html)


## Library
Library type: CQL, tekst, FHIRPath of SQL? (Voor nu tekst en SQL).

### Query (IBD Cohort):
*

### Used Profiles:
1. EpisodeOfCare (nog niet kloppend); [MyModelEpisodeOfCare](StructureDefinition-my-model-episode-of-care.html)
2. Procedure (nog niet kloppend); [MyModelProcedure](StructureDefinition-my-model-procedure.html) (of [SanteonProcedure](StructureDefinition-santeon-procedure.html))

### Aangeroepen Valuesets
1. **Zorgtype**: Alleen de codes `11` (Regulier) en `21` (Vervolg) worden verwerkt via de [ValueSet: LocalZorgtypeCodes](ValueSet-local-zorgtype-codes.html).
2. **SpecialismeDiagnoseCode**: Dit veld verwacht een gecombineerde string van de Vektis-zorgsoort en de NZa-hoofddiagnose conform de [ValueSet: LocalSpecialismeDiagnoseCodes](ValueSet-local-specialisme-diagnose-codes.html) (bijv. `0318601` voor MDL met diagnose 601).
3. **VerrichtingTypeCodeNZa**: Systemen moeten verplicht valideren tegen de [ValueSet: LocalVerrichtingCodesNZa](ValueSet-local-verrichting-codes-nza.html) om te bepalen of een verrichting telt als een geldige scopie (codes: `034620`, `034686`, `034690`, `035582`).


---
---
---
## Technische SQL Logica*
Hergebruikers van deze IG kunnen onderstaande SQL-query toepassen op hun relationele datamart (bijv. ChipSoft HiX EPD-tabellen) om de populatie te genereren:

```sql
SELECT
    DISTINCT e.PATIENTNR
FROM
    EPISODE_DBCPER d
    JOIN EPISODE_EPISODE e ON d.EPISODE = e.EPISODE
    JOIN CSZISLIB_SPEC s ON d.SPECIALISM = s.SPECCODE
    JOIN EPISODE_ZORGTYPE z ON d.ZORGTYPE = z.CODE
    JOIN PATIENT_PATIENT p ON e.PATIENTNR = p.PATIENTNR
WHERE 1=1
    AND d.BEGINDAT >= '2018-01-01'
    AND p.GEBDAT < DATEADD(year, -18, GETDATE())
    AND z.LANDELIJK IN ('11','21')
    AND d.VERVALLEN = 0
    AND (
            (CONCAT(s.ZORGVSOORT, FORMAT(s.COTGCODE, '00')) = '0313' AND d.HOOFDDIAG IN ('922','923'))
            OR
            (CONCAT(s.ZORGVSOORT, FORMAT(s.COTGCODE, '00')) = '0318' AND d.HOOFDDIAG IN ('601','602'))
            OR
            (CONCAT(s.ZORGVSOORT, FORMAT(s.COTGCODE, '00')) = '0303' AND d.HOOFDDIAG IN ('115','116','163','325','326'))
    )
```
<!--
## Semantische Mapping naar Zorginformatiebouwstenen (zib's)
Om hergebruik van data uit het Elektronisch Patiëntendossier (EPD) te garanderen, mapt deze indicator direct naar de Nederlandse zib-standaarden (versie 2020):

| Indicator Element | Relevante zib | zib Concept ID / Code | FHIR Resource Mapping |
| :--- | :--- | :--- | :--- |
| **CarePlan ZBJ_IBD** | [zib Behandeldoel-v1.0](https://zibs.nl) | `NL-CM:13.5.1` (Behandeldoel) | `CarePlan.category` |
| **DBC Diagnoses** | [zib Probleem-v4.4](https://zibs.nl) | `NL-CM:5.1.3` (ProbleemNaam) | `Condition` / `EpisodeOfCare.reason` |
| **NZa Verrichting** | [zib Verrichting-v5.3](https://zibs.nl) | `NL-CM:14.1.2` (VerrichtingType) | `Procedure.code` |
| **Geboortedatum** | [zib Patiënt-v3.2](https://zibs.nl) | `NL-CM:0.1.10` (Geboortedatum) | `Patient.birthDate` |

### Toelichting op Zorgtype & Specialisme
Binnen de Nederlandse wet- en regelgeving rondom medisch-specialistische zorg (MSZ) worden DBC-trajecten niet direct door zib-kernen gedekt. In deze IG worden deze conform de Nictiz-richtlijnen vertaald naar:
* **Specialisme**: Gekoppeld via de `Encounter.extension` of `EpisodeOfCare.team` met het zorgverleners-specialisme (Vektis codelijst `COD016-VEKT`).
* **Zorgtype**: Gekoppeld middels de Nederlandse extensie voor zorgtypen binnen het zorgtraject (`Encounter`).
-->

<!-- ## FHIR Resources in deze IG
De formele FHIR-definities zijn vastgelegd in de volgende opgestelde profielen en artefacten binnen deze IG:
* [[ValueSet-nza-scopie-verrichting-codes]] - De specifieke NZa verrichtingentabel-selectie.
* [[Library-ibd-scopie-indicator-logica]] - De technische SQL-vertaling voor EPD-systemen.
* [[Measure-ibd-scopie-cohort-measure]] - De formele cohortindicator-structuur. -->


---
## Directe Relatie met ons Informatiemodel (Database-to-FHIR)

<!-- SIMveld | Database Tabel & Kolom | Intern Informatiemodel Concept | FHIR Profiel Element |
| :--- | :--- | :--- | :--- |
DBC;DBCOpeningsDatum | `EPISODE_DBCPER.BEGINDAT` | DBC Begindatum | `MyModelEpisodeOfCare.period.start` [Santeon_DBCModel](StructureDefinition-my-model-episode-of-care.html)|
DBC;x | `EPISODE_DBCPER.VERVALLEN` | DBC Vervallen Status (0/1) | `MyModelEpisodeOfCare.extension[vervallen]` |
DBC;SpecialismeDiagnoseCode | `EPISODE_DBCPER.HOOFDDIAG` | Hoofddiagnose Code | `MyModelEpisodeOfCare.diagnosis.condition.extension[hoofddiag]` [Valueset-LocalSpecialismeDiagnoseCodes](Valueset-local-specialisme-diagnose-codes.html)|
Contact;? | `EPISODE_ZORGTYPE.CODE` | Landelijk Zorgtype (11/21) | `MyModelEncounter.type.coding.code` [Santeon_ContactModel](StructureDefinition-my-model-encounter.html) en [Valueset-LocalZorgtypeCodes](Valueset-local-zorgtype-codes.html)|
DBC;SpecialismeDiagnoseCode of ZorgverlenerRol;ZorgverlenerRolSpecialismeCode | `CSZISLIB_SPEC.SPECCODE` | Specialismecode (0313/0318/0303) | `MyModelEpisodeOfCare.type.text` |
Verrichting;VerrichtingTypeCodeNZa | `VERRICHTING.CODE` | NZa Verrichtingcode | `MyModelProcedure.code.coding.code` [Santeon_VerrichtingModel](StructureDefinition-my-model-procedure.html) en [Valueset-LocalVerrichtingCodesNZa](Valueset-local-verrichting-codes-nza.html)|
Patient;Geboortedatum | `PATIENT_PATIENT.GEBDAT` | Geboortedatum Patiënt | `Patient.birthDate` | -->

| SIMveld | Database Tabel & Kolom | Intern Informatiemodel Concept | FHIR Profiel Element & Pagina |
| :--- | :--- | :--- | :--- |
| DBC;DBCOpeningsDatum | `EPISODE_DBCPER.BEGINDAT` | DBC Begindatum | [MyModelEpisodeOfCare](StructureDefinition-my-model-episode-of-care.html) (`period.start`) |
| DBC;x | `EPISODE_DBCPER.VERVALLEN` | DBC Vervallen Status (0/1) | [MyModelEpisodeOfCare](StructureDefinition-my-model-episode-of-care.html) (`extension[vervallen]`) |
| DBC;SpecialismeDiagnoseCode | `EPISODE_DBCPER.HOOFDDIAG` | Hoofddiagnose Code | [ValueSet: LocalSpecialismeDiagnoseCodes](ValueSet-local-specialisme-diagnose-codes.html) via `extension[hoofddiag]` |
| DBC;ZorgTypeCode | `EPISODE_ZORGTYPE.CODE` | Landelijk Zorgtype (11/21) | [MyModelEpisodeOfCare](StructureDefinition-my-model-episode-of-care.html) en [ValueSet: LocalZorgtypeCodes](ValueSet-local-zorgtype-codes.html) |
| DBC;SpecialismeDiagnoseCode of ZorgverlenerRol;... | `CSZISLIB_SPEC.SPECCODE` | Specialismecode (0313/0318/0303) | [MyModelEpisodeOfCare](StructureDefinition-my-model-episode-of-care.html) (`type.text`) |
| Verrichting;VerrichtingTypeCodeNZa | `VERRICHTING.CODE` | NZa Verrichtingcode | [MyModelProcedure](StructureDefinition-my-model-procedure.html) en [ValueSet: LocalVerrichtingCodesNZa](ValueSet-local-verrichting-codes-nza.html) |
| Patient;Geboortedatum | `PATIENT_PATIENT.GEBDAT` | Geboortedatum Patiënt | `Patient.birthDate` |

<!--
## Technische Gegevensspecificatie (ValueSets)

Voor de juiste werking van de indicator moeten de gegevens uit het lokale informatiemodel exact overéénstemmen met de onderstaande FHIR ValueSets. 

### Gecodereerde Gegevensvelden

1. **Zorgtype**: Gekoppeld aan `MyModelEncounter`. Alleen de codes `11` (Regulier) en `21` (Vervolg) worden verwerkt via de ValueSet `local-zorgtype-codes`.
2. **SpecialismeDiagnoseCode**: Gekoppeld via de maatwerkextensie op `MyModelEpisodeOfCare`. Dit veld verwacht een gecombineerde string van de Vektis-zorgsoort en de NZa-hoofddiagnose conform de ValueSet `local-specialisme-diagnose-codes` (bijv. `0318601` voor MDL met diagnose 601).
3. **VerrichtingTypeCodeNZa**: Gekoppeld aan `MyModelProcedure.code`. Systemen moeten verplicht valideren tegen de ValueSet `local-verrichting-codes-nza` om te bepalen of een verrichting telt als een geldige scopie (codes: `034620`, `034686`, `034690`, `035582`).

## Hoe de Indicator werkt (Measure vs. Profielen)

Deze IG maakt een strikt onderscheid tussen de **Data-profielen** (hoe de data in de FHIR-server staat) en de **Indicator-definitie** (hoe het cohort berekend wordt).

1. **Brondata**: Ziekenhuizen transformeren hun lokale EPD-tabellen (`EPISODE_DBCPER`, `VERRICHTING`) naar de FHIR-profielen `MyModelEpisodeOfCare`, `MyModelEncounter` en `MyModelProcedure`.
2. **Aggregatie**: De [Measure_scopie_indicator](Measure-IbdScopieCohortMeasure.html) `MyModelIbdScopieMeasure` (=example...) resource bevat de centrale FHIRPath-formule. Wanneer een FHIR-indicator-engine deze `Measure` uitvoert over de brondata, valideert deze automatisch de leeftijd (18+), de NZa-verrichtingencodes en de DBC-koppelingen om tot het uiteindelijke patiënten-aantal te komen.

## Ondersteunde Aanlevermethoden

Om de implementatielast voor ziekenhuizen zo laag mogelijk te houden, ondersteunt deze Implementation Guide (IG) twee methoden om de IBD-scopie-indicator te berekenen. ICT-afdelingen kunnen zelf bepalen welke route het beste past bij hun huidige infrastructuur:

### Optie 1: De Lokale SQL-Route (Direct op de Database)
* **Voor wie**: Ziekenhuizen met een ChipSoft HiX-inrichting die snel resultaat willen zonder data eerst te transformeren naar losse FHIR-resources.
* **Werking**: De indicator-engine voert de SQL-query uit die is opgeslagen in `Library-ibd-scopie-indicator-logica`. Deze query rekent direct over de tabellen `EPISODE_DBCPER`, `EPISODE_EPISODE`, `CSZISLIB_SPEC`, `EPISODE_ZORGTYPE` en `PATIENT_PATIENT`.
* **Uitsluiting < 18 jaar**: Wordt binnen deze route afgehandeld via de `denominator-exclusion` criteria middels een SQL geboortedatum-check.

### Optie 2: De Generieke FHIRPath-Route (EPD-Onafhankelijk)
* **Voor wie**: Ziekenhuizen die werken met Epic, Nexus, of een centraal FHIR-dataplatform (VPI) en de data al ontsluiten via gestandaardiseerde zibs/profielen.
* **Werking**: De server evalueert de FHIRPath-expressie direct over de actieve FHIR-resources heen. Er is geen database-specifieke SQL vereist.
* **Profielen**: De data dient vooraf te zijn gemapt naar `MyModelProcedure`, `MyModelEncounter` en `MyModelEpisodeOfCare`.
* **Uitsluiting < 18 jaar**: Zit direct ingebakken in de hoofdexpressie (`Patient.birthDate <= today() - 18 years`).

-->