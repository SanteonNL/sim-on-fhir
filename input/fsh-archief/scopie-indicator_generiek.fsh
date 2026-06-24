Profile: MyModelEpisodeOfCare
Parent: EpisodeOfCare
Id: my-model-episode-of-care
Title: "MyModel: DBC Perifeer (EPISODE_DBCPER)"
Description: "Directe vertaling van de tabel EPISODE_DBCPER uit ons informatiemodel naar FHIR."
// Koppeling met de extensies uit uw model
* extension contains LocalDbcVervallen named vervallen 1..1
* extension[vervallen].valueInteger = 0  // Alleen niet-vervallen DBCs zijn toegestaan in dit profiel

// Specialisme (SPECIALISM) mapping
* type 1..1
* type.text = "Ingevuld met de s.SPECCODE / s.ZORGVSOORT uit ons informatiemodel"

// Hoofddiag (HOOFDDIAG) mapping via de diagnose component
* diagnosis 1..*
* diagnosis.condition only Reference(Condition)
* diagnosis.condition.extension contains LocalHoofddiag named hoofddiag 1..1

// Begindatum (BEGINDAT) mapping
* period.start 1..1

Profile: MyModelEncounter
Parent: Encounter
Id: my-model-encounter
Title: "MyModel: Subtraject / Zorgtype"
Description: "Vertaling van het contact en het zorgtype (ZORGTYPE) uit ons informatiemodel."
* episodeOfCare 1..1
* episodeOfCare only Reference(MyModelEpisodeOfCare)
// Zorgtype hard valideren op uw codes '11' of '21'
* type 1..1
* type.coding.code from LocalZorgtypeCodes (required)

Profile: MyModelProcedure
Parent: Procedure
Id: my-model-procedure
Title: "MyModel: Verrichting (VERRICHTING)"
Description: "Directe vertaling van de verrichtingen uit ons informatiemodel."
* status = #completed
* code 1..1
* code from LocalVerrichtingCodesNZa (required)
* performedDateTime 1..1


Extension: LocalDbcVervallen
Id: local-dbc-vervallen
Title: "Local Informatiemodel: DBC Vervallen Status"
Description: "Geeft aan of de DBC/DOT binnen het interne informatiemodel is vervallen (0 = actief, 1 = vervallen)."
* value[x] only integer

Extension: LocalHoofddiag
Id: local-hoofddiag
Title: "Local Informatiemodel: Hoofddiagnose Code"
Description: "Vastlegging van de specifieke NZa hoofddiagnosecode direct vanuit het bronsysteem."
* value[x] only string


ValueSet: LocalZorgtypeCodes
Id: local-zorgtype-codes
Title: "MyModel: Zorgtype Codes (11 en 21)"
Description: "Bevat de toegestane landelijke zorgtypes 11 en 21 uit de tabel EPISODE_ZORGTYPE."
* ^status = #active
* ^compose.include[0].system = "http://nictiz.nl"
* ^compose.include[0].concept[0].code = #11
* ^compose.include[0].concept[0].display = "Regulier DBC-zorgtraject"
* ^compose.include[0].concept[1].code = #21
* ^compose.include[0].concept[1].display = "Regulier vervolg-DBC-zorgtraject"

ValueSet: LocalSpecialismeDiagnoseCodes
Id: local-specialisme-diagnose-codes
Title: "MyModel: Specialisme Diagnose Codes (DBC/DOT)"
Description: "Geselecteerde combinaties van Specialisme (Zorgsoort) en NZa Hoofddiagnose uit uw informatiemodel."
* ^status = #active
* ^compose.include[0].system = "http://nictiz.nl"
* ^compose.include[0].concept[0].code = #922
* ^compose.include[0].concept[0].display = "Interne Geneeskunde - Diagnose 922"
* ^compose.include[0].concept[1].code = #923
* ^compose.include[0].concept[1].display = "Interne Geneeskunde - Diagnose 923"
* ^compose.include[0].concept[2].code = #601
* ^compose.include[0].concept[2].display = "Maag-Darm-Leverziekten - Diagnose 601"
* ^compose.include[0].concept[3].code = #602
* ^compose.include[0].concept[3].display = "Maag-Darm-Leverziekten - Diagnose 602"
* ^compose.include[0].concept[4].code = #115
* ^compose.include[0].concept[4].display = "Chirurgie - Diagnose 115"
* ^compose.include[0].concept[5].code = #116
* ^compose.include[0].concept[5].display = "Chirurgie - Diagnose 116"
* ^compose.include[0].concept[6].code = #163
* ^compose.include[0].concept[6].display = "Chirurgie - Diagnose 163"
* ^compose.include[0].concept[7].code = #325
* ^compose.include[0].concept[7].display = "Chirurgie - Diagnose 325"
* ^compose.include[0].concept[8].code = #326
* ^compose.include[0].concept[8].display = "Chirurgie - Diagnose 326"

ValueSet: LocalVerrichtingCodesNZa
Id: local-verrichting-codes-nza
Title: "MyModel: Verrichting Type Codes NZa (Scopie)"
Description: "De specifieke NZa-verrichtingencodes voor de scopie-indicator."
* ^status = #active
* ^compose.include[0].system = "http://nictiz.nl"
* ^compose.include[0].concept[0].code = #034620
* ^compose.include[0].concept[0].display = "Verrichting 034620"
* ^compose.include[0].concept[1].code = #034686
* ^compose.include[0].concept[1].display = "Verrichting 034686"
* ^compose.include[0].concept[2].code = #034690
* ^compose.include[0].concept[2].display = "Verrichting 034690"
* ^compose.include[0].concept[3].code = #035582
* ^compose.include[0].concept[3].display = "Verrichting 035582"


Instance: IbdScopieIndicatorLogica
InstanceOf: Library
Title: "SQL Logica voor IBD Scopie Indicator"
Description: "Bevat de SQL-query voor het selecteren van het IBD scopie-cohort op basis van DBC, CarePlan en verrichtingen."
* url = "https://ig.santeon.nl/ibd/Library/ibd-scopie-indicator-logica"
* version = "1.0.0"
* status = #active
* type = http://terminology.hl7.org/CodeSystem/library-type#logic-library "Logic Library"
* content.contentType = #text/sql
// * content.data = "SELECT DISTINCT e.PATIENTNR FROM EPISODE_DBCPER d JOIN EPISODE_EPISODE e ON d.EPISODE = e.EPISODE JOIN CSZISLIB_SPEC s ON d.SPECIALISM = s.SPECCODE JOIN EPISODE_ZORGTYPE z ON d.ZORGTYPE = z.CODE JOIN PATIENT_PATIENT p ON e.PATIENTNR = p.PATIENTNR WHERE d.BEGINDAT >= '2018-01-01' AND p.GEBDAT < DATEADD(year, -18, GETDATE()) AND z.LANDELIJK IN ('11','21') AND d.VERVALLEN = 0 AND ((CONCAT(s.ZORGVSOORT, FORMAT(s.COTGCODE, '00')) = '0313' AND d.HOOFDDIAG IN ('922','923')) OR (CONCAT(s.ZORGVSOORT, FORMAT(s.COTGCODE, '00')) = '0318' AND d.HOOFDDIAG IN ('601','602')) OR (CONCAT(s.ZORGVSOORT, FORMAT(s.COTGCODE, '00')) = '0303' AND d.HOOFDDIAG IN ('115','116','163','325','326')))"
// Onderstaande string is de gecontroleerde Base64-vertaling van uw SQL-query
* content.data = "U0VMRUNUIERJU1RJTkNUIGUuUEFUSUVOVE5SIEZST00gRVBJU09ERV9EQkNQRVIgZCBKT0lOIEVQSVNPREVfRVBJU09ERSBlIE9OIGQuRVBJU09ERSA9IGUuRVBJU09ERSBKT0lOIENTWklTTElCX1NQRUMgcyBPTiBkLlNQRUNJQUxJU00gPSBzLlNQRUNDT0RFIEpPSU4gRVBJU09ERV9aT1JHVFlQRSB6IE9OIGQuWk9SR1RZUEUgPSB6LkNPREUgSk9JTiBQQVRJRU5UX1BBVElFTlQgcCBPTiBlLlBBVElFTlROUiA9IHAuUEFUSUVOVE5SIFdIRVJFIGQuQkVHSU5EQVQgPj0gJzIwMTgtMDEtMDEnIEFORCBwLkdFQkRBVCA8IERBVEVBREQoeWVhciwgLTE4LCBHRVREQVRFKCkpIEFORCB6LkxBTkRFTElKSyBJTiAoJzExJywnMjEnKSBBTkQgZC5WRVJWQUxMRU4gPSAwIEFORCAoKENPTkNBVChzLlpPUkdWU09PUlQsIEZPUk1BVChzLkNPVEdDT0RFLCAnMDAnKSkgPSAnMDMxMycgQU5EIGQuSE9PRkRESUFHIElOICgnOTIyJywnOTIzJykpIE9SIChDT05DQVQocy5aT1JHVlNPT1JULCBGT1JNQVQocy5DT1RHQ09ERSwgJzAwJykpID0gJzAzMTgnIEFORCBkLkhPT0ZERElBRyBJTiAoJzYwMScsJzYwMicpKSBPUiAoQ09OQ0FUKHMuWk9SR1ZTT09SVCwgRk9STUFUKHMuQ09UR0NPREUsICcwMCcpKSA9ICcwMzAzJyBBTkQgZC5IT09GRERJQUcgSU4gKCcxMTUnLCcxMTYnLCcxNjMnLCczMjUnLCczMjYnKSkp"

Instance: IbdScopieCohortMeasure
InstanceOf: Measure
Title: "Inclusie Cohort Patiënten met Scopie (IBD)"
Description: "Kwaliteitsindicator die het aantal unieke volwassen IBD-patiënten telt met een scopie-verrichting vanaf 2018."
* url = "https://ig.santeon.nl/ibd/Measure/ibd-scopie-cohort-measure"
* version = "1.0.0"
* status = #active
* scoring = http://terminology.hl7.org/CodeSystem/measure-scoring#cohort "Cohort"
* library = "https://ig.santeon.nl/ibd/Library/ibd-scopie-indicator-logica"

* group[0].code.text = "Hoofdgroep IBD Scopies"

// Initial Population (Inclusie)
* group[0].population[0].id = "initial-population-criteria"
* group[0].population[0].code = http://terminology.hl7.org/CodeSystem/measure-population#initial-population "Initial Population"
* group[0].population[0].description = "Patiënten met CarePlan ZBJ_IBD óf specifieke DBC diagnoses (0318, 0313, 0303) én een NZa verrichting (034620, 034686, 034690, 035582) sinds 2018. via lokale SQL-tabellen óf via gestandaardiseerde FHIR-profielen (Procedure, Encounter, EpisodeOfCare)."

// Methode A: lokale SQL expressie (verwijst naar de library hierboven)
* group[0].population[0].criteria.language = #text/sql
* group[0].population[0].criteria.expression = "Inclusie via lokale EPD database-tabellen (zie Library). via DBC, CarePlan en Verrichting criteria."
// Methode B: de generieke FHIRpath express
* group[0].population[0].criteria.extension[0].url = "http://hl7.org/fhir/StructureDefinition/expression"
* group[0].population[0].criteria.extension[0].valueExpression.language = #text/fhirpath
* group[0].population[0].criteria.extension[0].valueExpression.expression = "Patient.birthDate <= today() - 18 years and Procedure.where(code.coding.code.memberOf('https://ig.santeon.nl/ibd/ValueSet/local-verrichting-codes-nza')).exists()"

// Denominator Exclusion (Exclusie criteria voor de SQL-route)
* group[0].population[1].id = "exclusion-criteria"
* group[0].population[1].code = http://terminology.hl7.org/CodeSystem/measure-population#denominator-exclusion "Denominator Exclusion"
* group[0].population[1].description = "Patiënten jonger dan 18 jaar op het moment van aanleveren. (alleen van toepassing bij SQL-route)."
* group[0].population[1].criteria.language = #text/sql
* group[0].population[1].criteria.expression = "SELECT PATIENTNR FROM PATIENT WHERE GEBDAT > DATEADD(year, -18, GETDATE())"


