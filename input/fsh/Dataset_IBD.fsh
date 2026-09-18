// Gehele Cohort
Logical: IBDCohort
Parent: Base
Id: IBDCohort
Title: "IBD Cohort"
Description: "De cohortselectie beschrijft de criteria voor het selecteren van patiënten en bijbehorende DBC's/zorgtrajecten voor het IBD-cohort."

* Patienten 1..* IBDCohortPatient "Patiënten die voldoen aan de cohortcriteria"
* DBCs 1..* IBDCohortDBC "DBC's/zorgtrajecten die voldoen aan de cohortcriteria"

// Onderliggende modellen 
Logical: IBDCohortPatient
Parent: SIMPatient
Id: IBDCohortPatient
Title: "IBD Cohort Patient"
Description: """Selectie van informatie voor de IBD cohort van Patient. 
- **FHIR Query**: GET [base]/Patient?
  birthdate=le[Peildatum-18J]
- **Dataselectie**:
  Patiënten die op de peildatum (moment van aanlevering) 18 jaar of ouder zijn.
- **Zie ook [mappings](StructureDefinition-IBDCohortPatient-mappings.html) voor mappingsdetails.**"""

* Geboortedatum 1..1
* Geboortedatum ^short = "Leeftijd op peildatum ≥ 18 jaar"

Logical: IBDCohortDBC
Parent: SIMDBC
Id: IBDCohortDBC
Title: "IBD Cohort DBC/Zorgtraject"
Description: """Selectie van informatie voor de IBD dataset van DBC/Zorgtraject.
- **FHIR Query**: 
  - GET [base]/EpisodeOfCare?
  type:in=ValueSet/LocalZorgtypeCodes&
  diagnosis:Condition.code:in=ValueSet/LocalSpecialismeDiagnoseCodes&
  date=ge2018-01-01
- **Dataselectie**:
  Een DBC/zorgtraject geopend op of na 01-01-2018, met zorgtype 11 of 21, die niet is vervallen en waarvan de gekoppelde specialismediagnose voorkomt in:
  - [LocalSpecialismeDiagnoseCodes](ValueSet-LocalSpecialismeDiagnoseCodes.html)
  - _evt. [LocalZorgtypeCodes](ValueSet-LocalZorgtypeCodes.html) voor zorgtype 11 of 21_
- **Zie ook [mappings](StructureDefinition-IBDCohortDBC-mappings.html) voor mappingsdetails.**"""

* OpeningsDatum 1..1
* OpeningsDatum ^short = "≥ 2018-01-01"

* Geldig 1..1
* Geldig ^short = "TRUE"

* ZorgType 1..1
* ZorgType ^short = "Zorgtype 11 of 21"

* SpecialismeDiagnose 1..1
* SpecialismeDiagnose ^short = "Alle waarden uit dataselectie ValueSet LocalSpecialismeDiagnoseCodes (zie beschrijving)"


// Gehele Dataset
Logical: IBDDataset
Parent: Base
Id: IBDDataset
Title: "IBD Dataset"
Description: "De datasetselectie beschrijft de criteria voor het selecteren van informatie uit onderliggende modellen voor de IBD-dataset."

* Metingen 0..* IBDAlgemeneMeting "Metingen die voldoen aan de datasetcriteria"
* Verrichtingen 0..* IBDVerrichting "Verrichtingen die voldoen aan de datasetcriteria"
* DBCs 0..* IBDZorgtrajectDBC "DBC's/zorgtrajecten die voldoen aan de datasetcriteria"

// Onderliggende modellen
Logical: IBDAlgemeneMeting
Parent: SIMAlgemeneMeting
Id: IBDAlgemeneMeting
Title: "IBD Dataset AlgemeneMeting"
Description: """Selectie van informatie voor de IBD dataset van AlgemeneMeting.
- **FHIR Query**: GET [base]/Observation? 
  code:in=ValueSet/IBDAlgemeneMetingen&
  date=ge2018-01-01
- **Dataselectie**:
  [IBDAlgemeneMetingen](ValueSet-IBDAlgemeneMetingen.html)
- **Zie ook [mappings](StructureDefinition-IBDAlgemeneMeting-mappings.html) voor mappingsdetails.**"""

// Selectie
* MetingNaam 1..1 
* MetingNaam ^short = "Alle waarden uit dataselectie ValueSet IBDAlgemeneMetingen (zie beschrijving)"
* MetingNaam ^definition = "De bijbehorende validatieregels zijn gedefinieerd in SIMAlgemeneMeting."
* MetingDatumTijd 1..1 
* MetingDatumTijd ^short = ">= 2018-01-01"

// Niet nodig voor deze dataset
// 0..0

// Verrichting IBD
Logical: IBDVerrichting
Parent: SIMVerrichting
Id: IBDVerrichting
Title: "IBD Dataset Verrichting"
Description: """Selectie van informatie voor de IBD dataset van Verrichting.
- **FHIR Query**: GET [base]/Procedure? 
  code:in=ValueSet/IBDVerrichtingenNZa&
  date=ge2018-01-01
- **Dataselectie**:
  [IBDVerrichtingenNZa](ValueSet-IBDVerrichtingenNZa.html)
- **Zie ook [mappings](StructureDefinition-IBDVerrichting-mappings.html) voor mappingsdetails.**"""

// Selectie
* VerrichtingType 1..1
* VerrichtingType ^short = "Alle waarden uit dataselectie ValueSet IBDVerrichtingenNZa (zie beschrijving)"
* VerrichtingType ^definition = "De bijbehorende validatieregels zijn gedefinieerd in SIMVerrichting."
* StartDatum 1..1
* StartDatum ^short = ">= 2018-01-01"

// IBDZorgtrajectDBC
Logical: IBDZorgtrajectDBC
Parent: SIMDBC
Id: IBDZorgtrajectDBC
Title: "IBD Dataset DBC/Zorgtraject"
Description: """Selectie van informatie voor de IBD dataset van DBC/Zorgtraject.
- **FHIR Query**: GET [base]/EpisodeOfCare?
  diagnosis:Condition.code:in=ValueSet/IBDSpecialismeDiagnoses&
  start?-date=ge2017-01-01
- **Dataselectie**:
  [IBDSpecialismeDiagnoses](ValueSet-IBDSpecialismeDiagnoses.html)
- **Zie ook [mappings](StructureDefinition-IBDZorgtrajectDBC-mappings.html) voor mappingsdetails.**"""

// Selectie
* OpeningsDatum 1..1
* OpeningsDatum ^short = ">= 2017-01-01"
* SpecialismeDiagnose 1..1
* SpecialismeDiagnose ^short = "Alle waarden uit dataselectie ValueSet IBDSpecialismeDiagnoses (zie beschrijving)"