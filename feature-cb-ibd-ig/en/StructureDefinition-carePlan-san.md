# CarePlan (SAN) - SIM on FHIR v0.1.0

## Resource Profile: CarePlan (SAN) 

**Usages:**

* This Profile is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-carePlan-san.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-carePlan-san.csv), [Excel](../StructureDefinition-carePlan-san.xlsx), [Schematron](../StructureDefinition-carePlan-san.sch) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "carePlan-san",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/carePlan-san",
  "version" : "0.1.0",
  "name" : "CarePlanSan",
  "title" : "CarePlan (SAN)",
  "status" : "draft",
  "date" : "2026-11-26T10:18:33+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Santeon FHIR R4-profiel voor zorgplannen (deelname aan een zorgpad of thuismonitoringprogramma), gebaseerd op het Santeon Informatiemodel (SIMCarePlan) en afgeleid van FHIR R4 CarePlan.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "CarePlanSanFromSIMCarePlan",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMCarePlan",
    "name" : "SIM Logical Model SIMCarePlan",
    "comment" : "Herkomst van de CarePlanSan-elementen in Zorgplan (SIMCarePlan), met de conversietoelichting. Afgeleid van SIMCarePlanToFHIR; wijzigingen alleen daar aanbrengen."
  },
  {
    "identity" : "workflow",
    "uri" : "http://hl7.org/fhir/workflow",
    "name" : "Workflow Pattern"
  },
  {
    "identity" : "rim",
    "uri" : "http://hl7.org/v3",
    "name" : "RIM Mapping"
  },
  {
    "identity" : "w5",
    "uri" : "http://hl7.org/fhir/fivews",
    "name" : "FiveWs Pattern Mapping"
  },
  {
    "identity" : "v2",
    "uri" : "http://hl7.org/v2",
    "name" : "HL7 v2 Mapping"
  }],
  "kind" : "resource",
  "abstract" : false,
  "type" : "CarePlan",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/CarePlan",
  "derivation" : "constraint",
  "differential" : {
    "element" : [{
      "id" : "CarePlan",
      "path" : "CarePlan"
    },
    {
      "id" : "CarePlan.identifier",
      "path" : "CarePlan.identifier",
      "mapping" : [{
        "identity" : "CarePlanSanFromSIMCarePlan",
        "map" : "SIMCarePlan.CarePlanID",
        "comment" : "Direct: samengestelde identifier (system|value); het waardedeel wordt gehasht volgens de ETL-specificatie."
      }]
    },
    {
      "id" : "CarePlan.status",
      "path" : "CarePlan.status",
      "mapping" : [{
        "identity" : "CarePlanSanFromSIMCarePlan",
        "map" : "SIMCarePlan.Status",
        "comment" : "Direct: SIM code wordt ongewijzigd overgenomen als FHIR code. De in de IBD-indicatoren gebruikte code active komt voor in FHIR request-status; aansluiting van overige SIM-statuscodes nog te beoordelen."
      }]
    },
    {
      "id" : "CarePlan.category",
      "path" : "CarePlan.category",
      "mapping" : [{
        "identity" : "CarePlanSanFromSIMCarePlan",
        "map" : "SIMCarePlan.Categorie",
        "comment" : "Direct: SIM CodeableConcept wordt overgenomen als FHIR CodeableConcept."
      }]
    },
    {
      "id" : "CarePlan.subject",
      "path" : "CarePlan.subject",
      "mapping" : [{
        "identity" : "CarePlanSanFromSIMCarePlan",
        "map" : "SIMCarePlan.PatientIdentificatienummer",
        "comment" : "Structuurconversie: Patient-reference op basis van identificatienummer."
      }]
    },
    {
      "id" : "CarePlan.period.start",
      "path" : "CarePlan.period.start",
      "mapping" : [{
        "identity" : "CarePlanSanFromSIMCarePlan",
        "map" : "SIMCarePlan.StartDatumTijd",
        "comment" : "Structuurconversie: SIM StartDatumTijd en EindDatumTijd vormen samen de FHIR Period."
      }]
    },
    {
      "id" : "CarePlan.period.end",
      "path" : "CarePlan.period.end",
      "mapping" : [{
        "identity" : "CarePlanSanFromSIMCarePlan",
        "map" : "SIMCarePlan.EindDatumTijd",
        "comment" : "Structuurconversie: SIM StartDatumTijd en EindDatumTijd vormen samen de FHIR Period."
      }]
    }]
  }
}

```
