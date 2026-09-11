//Logical: IBDCohortPatient
//Logical: IBDCohortDBC

Logical: IBDAlgemeneMeting
Parent: SIMAlgemeneMeting
Id: IBDAlgemeneMeting
Title: "IBD Dataset AlgemeneMeting"
Description: """Selectie van informatie voor de IBD dataset van AlgemeneMeting.
- **FHIR Query**: GET [base]/Observation? 
  code:in=ValueSet/IBDAlgemeneMetingen&
  date=ge2018-01-01
- **Dataselectie**:
  [IBDAlgemeneMetingen](ValueSet-IBDAlgemeneMetingen.html)"""

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
  [IBDVerrichtingenNZa](ValueSet-IBDVerrichtingenNZa.html)"""

// Selectie
* VerrichtingType 1..1
* VerrichtingType ^short = "Alle waarden uit dataselectie ValueSet IBDVerrichtingenNZa (zie beschrijving)"
* VerrichtingType ^definition = "uitleg validatie"
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
  [IBDSpecialismeDiagnoses](ValueSet-IBDSpecialismeDiagnoses.html)"""

// Selectie
* OpeningsDatum 1..1
* OpeningsDatum ^short = ">= 2017-01-01"
* SpecialismeDiagnose 1..1
* SpecialismeDiagnose ^short = "Alle waarden uit dataselectie ValueSet IBDSpecialismeDiagnoses (zie beschrijving)"