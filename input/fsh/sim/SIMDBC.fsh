Logical: SIMDBC
Parent: Base
Id: SIMDBC
Title: "DBC/Zorgtraject (model)"
Description: "Logisch model voor een DBC binnen het Santeon Informatiemodel."

* Patient 0..1 SIMPatient "Patiënt waarop de DBC betrekking heeft"
* SubtrajectNummer 0..1 Identifier "Uniek identificatienummer van het DBC-subtraject"
* ZorgtrajectNummer 0..1 Identifier "Identificatienummer van het zorgtraject"
* Diagnose 0..1 string "Diagnosecode en omschrijving"
* SpecialismeDiagnose 0..1 CodeableConcept "Specialisme en diagnose behorend bij de DBC"
* SpecialismeDiagnose from https://ig.santeon.nl/sim-on-fhir/ValueSet/Specialisme_DiagnoseCodelijstDummy (required)
* ZorgType 0..1 CodeableConcept "Zorgtype van de DBC"
* ZorgProduct 0..1 CodeableConcept "Zorgproduct van de DBC"
* Gevuld 0..1 boolean "Geeft aan of de DBC verrichtingen bevat"
* Geldig 0..1 boolean "Geeft aan of de DBC geldig is"
* OpeningsDatum 0..1 date "Openingsdatum van de DBC"
* SluitingsDatum 0..1 date "Sluitingsdatum van de DBC"
* UitvoerderSpecialisme 0..1 Coding "Specialisme van de uitvoerder"
* Verwijzer 0..1 CodeableConcept "Oorsprong van de verwijzing"

Mapping: SIMDBCFromSIMCSV
Id: SIMDBCFromSIMCSV
Title: "SIM CSVs"
Source: SIMDBC
Target: "SIM CSVs"

* Patient -> "Identificatienummer"

* SubtrajectNummer -> "SubtrajectNr"
* ZorgtrajectNummer -> "ZorgtrajectNr"

* SpecialismeDiagnose.coding.system -> "SpecialismeDiagnoseSysteem"
* SpecialismeDiagnose.coding.code -> "SpecialismeDiagnoseCode"
* SpecialismeDiagnose.coding.display -> "SpecialismeDiagnoseOmschrijving"

* ZorgType.coding.system -> "ZorgTypeCodeSysteem"
* ZorgType.coding.code -> "ZorgTypeCode"
* ZorgType.coding.display -> "ZorgTypeOmschrijving"

* ZorgProduct.coding.system -> "ZorgProductCodeSysteem"
* ZorgProduct.coding.code -> "ZorgProductCode"
* ZorgProduct.coding.display -> "ZorgProductOmschrijving"

* Gevuld -> "DBCgevuld"
* Geldig -> "DBCgeldig"

* OpeningsDatum -> "DBCOpeningsDatum"
* SluitingsDatum -> "DBCSluitingsDatum"

* UitvoerderSpecialisme.system -> "Uitvoerder_SpecialismeSysteem"
* UitvoerderSpecialisme.code -> "Uitvoerder_Specialisme"

* Verwijzer.coding.system -> "DBCverwijzerCodeSysteem"
* Verwijzer.coding.code -> "DBCverwijzerCode"
* Verwijzer.coding.display -> "DBCverwijzerCodeOmschrijving"

Mapping: SIMDBCToFHIR
Id: SIMDBCToFHIR
Title: "SIM DBC naar FHIR"
Source: SIMDBC
Target: "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/episodeOfCare-san"
//http://hl7.org/fhir/StructureDefinition/episodeOfCare-san"

* Patient -> "EpisodeOfCare.patient"
* SubtrajectNummer -> "EpisodeOfCare.identifier"
// let op: vereist transformatielogica
* Geldig -> "EpisodeOfCare.status"
* OpeningsDatum -> "EpisodeOfCare.period.start"
* SluitingsDatum -> "EpisodeOfCare.period.end"
* ZorgType -> "EpisodeOfCare.type"
// de diagnose wordt gerepresenteerd in Condition.code (via EpisodeOfCare.diagnosis.condition)
* SpecialismeDiagnose -> "EpisodeOfCare.diagnosis.condition"