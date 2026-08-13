# Afwijkingen ten opzichte van het huidige SIM

Dit document beschrijft de afwijkingen ten opzichte van het huidige SIM die voortkomen uit de modellering op basis van FHIR-resources.

> Vraag: willen we SOF modelleren adhv het SIM (met bv. veel extensies) of adhv FHIR met hermodelleer suggesties (als gevolg dat er SIM wijzingen gaan zijn)?

## Afwijkingen

- *DBC;DiagnoseCode (912 string), 
- *DBC;uitvoerder_specialisme (0313 AGBCodelijst), 
- DBC;SpecialismeDiagnoseCode (0313|912 Specialisme_DiagnoseCodelijst)
    - EpisodeOfCare ─ .diagnosis → ConditionSan (code ∈ ValueSet: LocalSpecialismeDiagnoseCodes OF DiagnoseCode) evt. met role = billing.
    - bepalen of Uitvoerder_Specialisme daarnaast afzonderlijk op EpisodeOfCareSan moet worden gemodelleerd, bijvoorbeeld via een extension.
- Weten we welke zorgverlener/rol verantwoordelijk was? --> PractitionerRole -> .specialty

## Hermodelleer suggesties
- DBC: zie FHIR termen*

## FHIR termen*
| Resource | Ontwerpkeuze | R4(/5) |
| :--- | :--- | :--- |
| [Account](https://hl7.org/fhir/R4/account.html) |  DBC-traject financieel | The Account resource can be considered a "bucket" to which ChargeItem resources are linked
[ChargeItem](https://hl7.org/fhir/R4/chargeitem.html) | Zorgactiviteiten | References to Encounter/EpisodeOfCare, Patient/Group and Services provide further context to help billing systems determine the appropriate account and establish the clinical/financial context to evaluate the rules associated with the charge codes. |
[Claim](https://hl7.org/fhir/R4/claim.html) | uiteindelijke DBC/Factuur | To exchange the financial information. A suite of goods and services and insurances coverages under which adjudication or authorization is requested. |
[EpisodeOfCare](https://hl7.org/fhir/R4/episodeofcare.html) | DBC-traject medisch | **Systems collect a coherent group of activities (such as encounters) related to a patient's health condition or problem often referred to as a Care Episode.** The primary difference between the EpisodeOfCare and the Encounter is that the Encounter records the details of an activity directly relating to the patient, while the EpisodeOfCare is the container that can link a series of Encounters together for problems/issues. This difference is a similar difference between the EpisodeOfCare and a CarePlan. The EpisodeOfCare is a tracking resource, rather than a planning resource. An EpisodeOfCare contains details about the purpose of the care and can exist without any activities. The minimal information that would be required in an episode of care would be a patient, organization and a reason for the ongoing association. Other reasons for creating an EpisodeOfCare could be for tracking the details required for government reporting or billing. |
[Encounter](https://hl7.org/fhir/R4/encounter.html#Encounter) | Contact | An interaction during which services are provided to the patient. |
