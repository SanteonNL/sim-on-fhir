# Contact (model) - SIM on FHIR v0.1.0

## Logical Model: Contact (model) 

**Usages:**

* This Logical Model is not used by any profiles in this Specification

You can also check for [usages in the FHIR IG Statistics](https://packages2.fhir.org/xig/resource/santeon.sim-on-fhir|current/StructureDefinition/StructureDefinition-SIMContact.json)

### Formal Views of Profile Content

 [Description Differentials, Snapshots, and other representations](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#structure-definitions). 

 

Other representations of profile: [CSV](../StructureDefinition-SIMContact.csv), [Excel](../StructureDefinition-SIMContact.xlsx) 



## Resource Content

```json
{
  "resourceType" : "StructureDefinition",
  "id" : "SIMContact",
  "url" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMContact",
  "version" : "0.1.0",
  "name" : "SIMContact",
  "title" : "Contact (model)",
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
  "description" : "Generiek logisch model voor een zorgcontact binnen het Santeon Informatiemodel.",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "fhirVersion" : "4.0.1",
  "mapping" : [{
    "identity" : "SIMContactFromSIMCSV",
    "uri" : "urn:santeon:sim:csv",
    "name" : "SIM CSV"
  },
  {
    "identity" : "SIMContactToFHIR",
    "uri" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/encounter-san",
    "name" : "SIMContact naar FHIR"
  }],
  "kind" : "logical",
  "abstract" : false,
  "type" : "https://ig.santeon.nl/sim-on-fhir/StructureDefinition/SIMContact",
  "baseDefinition" : "http://hl7.org/fhir/StructureDefinition/Base",
  "derivation" : "specialization",
  "differential" : {
    "element" : [{
      "id" : "SIMContact",
      "path" : "SIMContact",
      "short" : "Contact (model)",
      "definition" : "Generiek logisch model voor een zorgcontact binnen het Santeon Informatiemodel."
    },
    {
      "id" : "SIMContact.ContactID",
      "path" : "SIMContact.ContactID",
      "short" : "Uniek ID (key) binnen patiënt per contact",
      "definition" : "Uniek ID (key) binnen patiënt per contact",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMContactFromSIMCSV",
        "map" : "Contact;ContactID"
      },
      {
        "identity" : "SIMContactToFHIR",
        "map" : "Encounter.identifier"
      }]
    },
    {
      "id" : "SIMContact.VerrichtingID",
      "path" : "SIMContact.VerrichtingID",
      "short" : "Uniek ID (key) binnen patiënt per verrichting",
      "definition" : "Uniek ID (key) binnen patiënt per verrichting",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMContactFromSIMCSV",
        "map" : "Contact;VerrichtingID"
      }]
    },
    {
      "id" : "SIMContact.SubtrajectNummer",
      "path" : "SIMContact.SubtrajectNummer",
      "short" : "Uniek ID (key) binnen patiënt per DBC-subtraject",
      "definition" : "Uniek ID (key) binnen patiënt per DBC-subtraject",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMContactFromSIMCSV",
        "map" : "Contact;SubtrajectNr"
      },
      {
        "identity" : "SIMContactToFHIR",
        "map" : "Encounter.episodeOfCare",
        "comment" : "Indien het DBC-subtraject als EpisodeOfCare wordt gerepresenteerd."
      }]
    },
    {
      "id" : "SIMContact.PatientIdentificatienummer",
      "path" : "SIMContact.PatientIdentificatienummer",
      "short" : "Identificatienummer van de patiënt",
      "definition" : "Identificatienummer van de patiënt",
      "min" : 1,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMContactFromSIMCSV",
        "map" : "Contact;Identificatienummer"
      },
      {
        "identity" : "SIMContactToFHIR",
        "map" : "Encounter.subject",
        "comment" : "Patient-reference op basis van identificatienummer."
      }]
    },
    {
      "id" : "SIMContact.ContactType",
      "path" : "SIMContact.ContactType",
      "short" : "Type zorgcontact, bijvoorbeeld poliklinisch, klinisch of spoedeisend",
      "definition" : "Type zorgcontact, bijvoorbeeld poliklinisch, klinisch of spoedeisend",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Coding"
      }],
      "mapping" : [{
        "identity" : "SIMContactToFHIR",
        "map" : "Encounter.class"
      }]
    },
    {
      "id" : "SIMContact.ContactType.system",
      "path" : "SIMContact.ContactType.system",
      "mapping" : [{
        "identity" : "SIMContactFromSIMCSV",
        "map" : "Contact;ContactTypeCodeSysteem"
      }]
    },
    {
      "id" : "SIMContact.ContactType.code",
      "path" : "SIMContact.ContactType.code",
      "mapping" : [{
        "identity" : "SIMContactFromSIMCSV",
        "map" : "Contact;ContactTypeCode"
      }]
    },
    {
      "id" : "SIMContact.ContactType.display",
      "path" : "SIMContact.ContactType.display",
      "mapping" : [{
        "identity" : "SIMContactFromSIMCSV",
        "map" : "Contact;ContactTypeOmschrijving"
      }]
    },
    {
      "id" : "SIMContact.ContactPrioriteit",
      "path" : "SIMContact.ContactPrioriteit",
      "short" : "Urgentie van het zorgcontact",
      "definition" : "Urgentie van het zorgcontact",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }],
      "binding" : {
        "strength" : "example",
        "valueSet" : "http://terminology.hl7.org/ValueSet/v3-ActPriority"
      },
      "mapping" : [{
        "identity" : "SIMContactToFHIR",
        "map" : "Encounter.priority"
      }]
    },
    {
      "id" : "SIMContact.ContactPrioriteit.coding.system",
      "path" : "SIMContact.ContactPrioriteit.coding.system",
      "mapping" : [{
        "identity" : "SIMContactFromSIMCSV",
        "map" : "Contact;EncounterPriorityCodingSystem"
      }]
    },
    {
      "id" : "SIMContact.ContactPrioriteit.coding.code",
      "path" : "SIMContact.ContactPrioriteit.coding.code",
      "mapping" : [{
        "identity" : "SIMContactFromSIMCSV",
        "map" : "Contact;EncounterPriorityCodingCode"
      }]
    },
    {
      "id" : "SIMContact.ContactPrioriteit.coding.display",
      "path" : "SIMContact.ContactPrioriteit.coding.display",
      "mapping" : [{
        "identity" : "SIMContactFromSIMCSV",
        "map" : "Contact;EncounterPriorityCodingDisplay"
      }]
    },
    {
      "id" : "SIMContact.Bestemming",
      "path" : "SIMContact.Bestemming",
      "short" : "Bestemming van de patiënt na het zorgcontact",
      "definition" : "Bestemming van de patiënt na het zorgcontact",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "CodeableConcept"
      }],
      "mapping" : [{
        "identity" : "SIMContactToFHIR",
        "map" : "Encounter.hospitalization.dischargeDisposition",
        "comment" : "Wanneer het gegeven de ontslagbestemming beschrijft."
      }]
    },
    {
      "id" : "SIMContact.Bestemming.coding.system",
      "path" : "SIMContact.Bestemming.coding.system",
      "mapping" : [{
        "identity" : "SIMContactFromSIMCSV",
        "map" : "Contact;BestemmingCodeSysteem"
      }]
    },
    {
      "id" : "SIMContact.Bestemming.coding.code",
      "path" : "SIMContact.Bestemming.coding.code",
      "mapping" : [{
        "identity" : "SIMContactFromSIMCSV",
        "map" : "Contact;BestemmingCode"
      }]
    },
    {
      "id" : "SIMContact.Bestemming.coding.display",
      "path" : "SIMContact.Bestemming.coding.display",
      "mapping" : [{
        "identity" : "SIMContactFromSIMCSV",
        "map" : "Contact;BestemmingOmschrijving"
      }]
    },
    {
      "id" : "SIMContact.BeginDatumTijd",
      "path" : "SIMContact.BeginDatumTijd",
      "short" : "Datum en eventueel tijd waarop het contact is begonnen",
      "definition" : "Datum en eventueel tijd waarop het contact is begonnen",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "SIMContactFromSIMCSV",
        "map" : "Contact;BeginDatumTijd"
      },
      {
        "identity" : "SIMContactToFHIR",
        "map" : "Encounter.period.start"
      }]
    },
    {
      "id" : "SIMContact.EindDatumTijd",
      "path" : "SIMContact.EindDatumTijd",
      "short" : "Datum en eventueel tijd waarop het contact is beëindigd",
      "definition" : "Datum en eventueel tijd waarop het contact is beëindigd",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "dateTime"
      }],
      "mapping" : [{
        "identity" : "SIMContactFromSIMCSV",
        "map" : "Contact;EindDatumTijd"
      },
      {
        "identity" : "SIMContactToFHIR",
        "map" : "Encounter.period.end"
      }]
    },
    {
      "id" : "SIMContact.ContactMet",
      "path" : "SIMContact.ContactMet",
      "short" : "Specialisme dat verantwoordelijk is voor het contact",
      "definition" : "Specialisme dat verantwoordelijk is voor het contact",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Coding"
      }]
    },
    {
      "id" : "SIMContact.ContactMet.system",
      "path" : "SIMContact.ContactMet.system",
      "mapping" : [{
        "identity" : "SIMContactFromSIMCSV",
        "map" : "Contact;ContactMetSysteem"
      }]
    },
    {
      "id" : "SIMContact.ContactMet.code",
      "path" : "SIMContact.ContactMet.code",
      "mapping" : [{
        "identity" : "SIMContactFromSIMCSV",
        "map" : "Contact;ContactMet"
      }]
    },
    {
      "id" : "SIMContact.AfspraakID",
      "path" : "SIMContact.AfspraakID",
      "short" : "Identificatienummer van de afspraak",
      "definition" : "Identificatienummer van de afspraak",
      "min" : 0,
      "max" : "1",
      "type" : [{
        "code" : "Identifier"
      }],
      "mapping" : [{
        "identity" : "SIMContactFromSIMCSV",
        "map" : "Contact;AfspraakID"
      },
      {
        "identity" : "SIMContactToFHIR",
        "map" : "Encounter.appointment",
        "comment" : "Appointment-reference op basis van AfspraakID."
      }]
    }]
  }
}

```
