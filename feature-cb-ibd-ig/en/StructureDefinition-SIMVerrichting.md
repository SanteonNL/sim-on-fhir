# Verrichting (model) - SIM on FHIR v0.1.0

## Logical Model: Verrichting (model) 

**Usages:**

* This Logical Model is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-SIMVerrichting.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-SIMVerrichting.csv), [Excel](../StructureDefinition-SIMVerrichting.xlsx) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "SIMVerrichting",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMVerrichting",
  "version" : "0.1.0",
  "name" : "SIMVerrichting",
  "title" : "Verrichting (model)",
  "status" : "draft",
  "date" : "2026-10-29T22:11:52+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "Generiek logisch model voor een verrichting binnen het Santeon Informatiemodel.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "SIMVerrichtingFromSIMCSV",
    "uri" : "urn:santeon:sim:csv",
    "name" : "SIM CSV"
  },
  {
    "identity" : "SIMVerrichtingToFHIR",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/procedure-san",
    "name" : "SIMVerrichting naar FHIR"
  },
  {
    "identity" : "SIMVerrichtingToGBB",
    "uri" : "http://ig.nictiz.nl/gbb/StructureDefinition/LogicalModelProcedure",
    "name" : "SIMVerrichting naar Nictiz GBB Procedure"
  }],
  "kind" : "logical",
  "abstract" : false,
  "type" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMVerrichting",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Base",
  "derivation" : "specialization",
  "differential" : {
    "element" : [{
      "id" : "SIMVerrichting",
      "path" : "SIMVerrichting",
      "short" : "Verrichting (model)",
      "definition" : "Generiek logisch model voor een verrichting binnen het Santeon Informatiemodel."
    },
    {
      "id" : "SIMVerrichting.VerrichtingID",
      "path" : "SIMVerrichting.VerrichtingID",
      "short" : "Uniek ID (key) binnen patiënt per verrichting",
      "definition" : "Uniek ID (key) binnen patiënt per verrichting",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMVerrichtingFromSIMCSV",
        "map" : "Verrichting;VerrichtingID"
      },
      {
        "identity" : "SIMVerrichtingToFHIR",
        "map" : "Procedure.identifier"
      },
      {
        "identity" : "SIMVerrichtingToGBB",
        "map" : "Procedure.feederAuditOriginatingSystemItemIds"
      }]
    },
    {
      "id" : "SIMVerrichting.ContactID",
      "path" : "SIMVerrichting.ContactID",
      "short" : "Uniek ID (key) binnen patiënt per contact",
      "definition" : "Uniek ID (key) binnen patiënt per contact",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMVerrichtingFromSIMCSV",
        "map" : "Verrichting;ContactID"
      },
      {
        "identity" : "SIMVerrichtingToFHIR",
        "map" : "Procedure.encounter",
        "comment" : "Encounter-reference op basis van ContactID."
      }]
    },
    {
      "id" : "SIMVerrichting.OperatieID",
      "path" : "SIMVerrichting.OperatieID",
      "short" : "Uniek ID (key) binnen patiënt per OK",
      "definition" : "Uniek ID (key) binnen patiënt per OK",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMVerrichtingFromSIMCSV",
        "map" : "Verrichting;OperatieID"
      }]
    },
    {
      "id" : "SIMVerrichting.SubtrajectNummer",
      "path" : "SIMVerrichting.SubtrajectNummer",
      "short" : "Identificatienummer van het DBC-subtraject",
      "definition" : "Identificatienummer van het DBC-subtraject",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMVerrichtingFromSIMCSV",
        "map" : "Verrichting;SubtrajectNr"
      }]
    },
    {
      "id" : "SIMVerrichting.PatientIdentificatienummer",
      "path" : "SIMVerrichting.PatientIdentificatienummer",
      "short" : "Identificatienummer van de patiënt",
      "definition" : "Identificatienummer van de patiënt",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMVerrichtingFromSIMCSV",
        "map" : "Verrichting;Identificatienummer"
      },
      {
        "identity" : "SIMVerrichtingToFHIR",
        "map" : "Procedure.subject",
        "comment" : "Patient-reference op basis van identificatienummer."
      },
      {
        "identity" : "SIMVerrichtingToGBB",
        "map" : "Procedure.subject.patient.patientIdentificationNumber",
        "comment" : "Koppeling naar de patiëntidentificatie binnen GBB Patient."
      }]
    },
    {
      "id" : "SIMVerrichting.VerrichtingType",
      "path" : "SIMVerrichting.VerrichtingType",
      "short" : "Type verrichting",
      "definition" : "Type verrichting, gecodeerd met NZa, SNOMED CT en/of DHD. Meerdere coderingen worden alleen gecombineerd wanneer zij hetzelfde verrichtingsconcept vertegenwoordigen.",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }],
      "mapping" : [{
        "identity" : "SIMVerrichtingToFHIR",
        "map" : "Procedure.code",
        "comment" : "Coderingen combineren wanneer zij hetzelfde concept vertegenwoordigen."
      },
      {
        "identity" : "SIMVerrichtingToGBB",
        "map" : "Procedure.procedureName",
        "comment" : "Alleen passende coderingen; GBB gebruikt een verplichte SNOMED-ValueSet."
      }]
    },
    {
      "id" : "SIMVerrichting.VerrichtingType.coding.system",
      "path" : "SIMVerrichting.VerrichtingType.coding.system",
      "mapping" : [{
        "identity" : "SIMVerrichtingFromSIMCSV",
        "map" : "Verrichting;VerrichtingTypeCodeNZaSysteem, Verrichting;VerrichtingTypeCodeSnomedCTSysteem, Verrichting;VerrichtingTypeCodeDHDSysteem"
      }]
    },
    {
      "id" : "SIMVerrichting.VerrichtingType.coding.code",
      "path" : "SIMVerrichting.VerrichtingType.coding.code",
      "mapping" : [{
        "identity" : "SIMVerrichtingFromSIMCSV",
        "map" : "Verrichting;VerrichtingTypeCodeNZa, Verrichting;VerrichtingTypeCodeSnomedCT, Verrichting;VerrichtingTypeCodeDHD"
      }]
    },
    {
      "id" : "SIMVerrichting.VerrichtingType.coding.display",
      "path" : "SIMVerrichting.VerrichtingType.coding.display",
      "mapping" : [{
        "identity" : "SIMVerrichtingFromSIMCSV",
        "map" : "Verrichting;VerrichtingTypeOmschrijvingNZa, Verrichting;VerrichtingTypeOmschrijvingSnomedCT, Verrichting;VerrichtingTypeOmschrijvingDHD"
      }]
    },
    {
      "id" : "SIMVerrichting.VerrichtingAantal",
      "path" : "SIMVerrichting.VerrichtingAantal",
      "short" : "Aantal verrichtingen",
      "definition" : "Aantal verrichtingen",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "integer"
      }],
      "mapping" : [{
        "identity" : "SIMVerrichtingFromSIMCSV",
        "map" : "Verrichting;VerrichtingAantal"
      }]
    },
    {
      "id" : "SIMVerrichting.VerrichtingStartDatum",
      "path" : "SIMVerrichting.VerrichtingStartDatum",
      "short" : "Datum en eventueel tijd waarop de verrichting is gestart",
      "definition" : "Datum en eventueel tijd waarop de verrichting is gestart",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "SIMVerrichtingFromSIMCSV",
        "map" : "Verrichting;VerrichtingStartDatum"
      },
      {
        "identity" : "SIMVerrichtingToFHIR",
        "map" : "Procedure.performedPeriod.start",
        "comment" : "Bij een verrichting met tijdsduur; anders performedDateTime."
      },
      {
        "identity" : "SIMVerrichtingToGBB",
        "map" : "Procedure.startDateTime"
      }]
    },
    {
      "id" : "SIMVerrichting.AanvragerSpecialisme",
      "path" : "SIMVerrichting.AanvragerSpecialisme",
      "short" : "Medisch specialisme dat de verrichting heeft aangevraagd",
      "definition" : "Medisch specialisme dat de verrichting heeft aangevraagd",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Coding"
      }]
    },
    {
      "id" : "SIMVerrichting.AanvragerSpecialisme.code",
      "path" : "SIMVerrichting.AanvragerSpecialisme.code",
      "mapping" : [{
        "identity" : "SIMVerrichtingFromSIMCSV",
        "map" : "Verrichting;Aanvrager_Specialisme"
      }]
    },
    {
      "id" : "SIMVerrichting.UitvoerderSpecialisme",
      "path" : "SIMVerrichting.UitvoerderSpecialisme",
      "short" : "Medisch specialisme dat de verrichting heeft uitgevoerd",
      "definition" : "Medisch specialisme dat de verrichting heeft uitgevoerd",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Coding"
      }]
    },
    {
      "id" : "SIMVerrichting.UitvoerderSpecialisme.code",
      "path" : "SIMVerrichting.UitvoerderSpecialisme.code",
      "mapping" : [{
        "identity" : "SIMVerrichtingFromSIMCSV",
        "map" : "Verrichting;Uitvoerder_Specialisme"
      }]
    },
    {
      "id" : "SIMVerrichting.VerrichtingCategorie",
      "path" : "SIMVerrichting.VerrichtingCategorie",
      "short" : "Duiding tot welke zorgprofielklasse een verrichting behoort",
      "definition" : "Duiding tot welke zorgprofielklasse een verrichting behoort",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }],
      "mapping" : [{
        "identity" : "SIMVerrichtingToFHIR",
        "map" : "Procedure.category"
      },
      {
        "identity" : "SIMVerrichtingToGBB",
        "map" : "Procedure.procedureType",
        "comment" : "Semantische en terminologische overeenstemming controleren."
      }]
    },
    {
      "id" : "SIMVerrichting.VerrichtingCategorie.coding.code",
      "path" : "SIMVerrichting.VerrichtingCategorie.coding.code",
      "mapping" : [{
        "identity" : "SIMVerrichtingFromSIMCSV",
        "map" : "Verrichting;VerrichtingCategorieCode"
      }]
    }]
  }
}

```
