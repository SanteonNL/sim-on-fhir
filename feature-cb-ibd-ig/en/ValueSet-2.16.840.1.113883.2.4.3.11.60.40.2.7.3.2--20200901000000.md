# AlcoholGebruikStatusCodelijst - SIM on FHIR v0.1.0

## ValueSet: AlcoholGebruikStatusCodelijst 

 **References** 

This value set is not used here; it may be used elsewhere (e.g. specifications and/or implementations that use this content)

### Logical Definition (CLD)

 

### Expansion

-------

 [Description of the above table(s)](http://build.fhir.org/ig/FHIR/ig-guidance/readingIgs.html#terminology). 



## Resource Content

```json
{
  "resourceType" : "ValueSet",
  "id" : "2.16.840.1.113883.2.4.3.11.60.40.2.7.3.2--20200901000000",
  "meta" : {
    "profile" : ["http://hl7.org/fhir/StructureDefinition/shareablevalueset"],
    "tag" : [{
      "system" : "http://hl7.org/fhir/FHIR-version",
      "code" : "4.0.1"
    }]
  },
  "extension" : [{
    "url" : "http://hl7.org/fhir/StructureDefinition/resource-effectivePeriod",
    "valuePeriod" : {
      "start" : "2020-09-01T00:00:00Z"
    }
  },
  {
    "url" : "http://hl7.org/fhir/StructureDefinition/web-source",
    "valueUrl" : "http://decor.nictiz.nl/decor/services/RetrieveValueSet?prefix=zib2020bbr-&version=&id=2.16.840.1.113883.2.4.3.11.60.40.2.7.3.2&effectiveDate=2020-09-01T00:00:00"
  }],
  "url" : "http://decor.nictiz.nl/fhir/ValueSet/2.16.840.1.113883.2.4.3.11.60.40.2.7.3.2--20200901000000",
  "identifier" : [{
    "use" : "official",
    "system" : "urn:ietf:rfc:3986",
    "value" : "urn:oid:2.16.840.1.113883.2.4.3.11.60.40.2.7.3.2"
  }],
  "version" : "0.1.0",
  "name" : "AlcoholGebruikStatusCodelijst",
  "title" : "AlcoholGebruikStatusCodelijst",
  "status" : "active",
  "experimental" : false,
  "date" : "2026-10-16T14:27:57+00:00",
  "publisher" : "Santeon",
  "contact" : [{
    "name" : "Santeon",
    "telecom" : [{
      "system" : "url",
      "value" : "https://www.santeon.nl"
    }]
  }],
  "description" : "AlcoholGebruikStatusCodelijst",
  "jurisdiction" : [{
    "coding" : [{
      "system" : "urn:iso:std:iso:3166",
      "code" : "NL",
      "display" : "Netherlands"
    }]
  }],
  "immutable" : false,
  "copyright" : "This artefact includes content from SNOMED Clinical Terms® (SNOMED CT®) which is copyright of the International Health Terminology Standards Development Organisation (IHTSDO). Implementers of these artefacts must have the appropriate SNOMED CT Affiliate license - for more information contact http://www.snomed.org/snomed-ct/getsnomed-ct or info@snomed.org.",
  "compose" : {
    "inactive" : true,
    "include" : [{
      "system" : "http://terminology.hl7.org/CodeSystem/v3-NullFlavor",
      "concept" : [{
        "code" : "OTH",
        "display" : "Other",
        "designation" : [{
          "language" : "en-US",
          "use" : {
            "system" : "http://snomed.info/sct",
            "code" : "900000000000013009",
            "display" : "Synonym"
          },
          "value" : "Other"
        },
        {
          "language" : "nl-NL",
          "use" : {
            "system" : "http://snomed.info/sct",
            "code" : "900000000000013009",
            "display" : "Synonym"
          },
          "value" : "Anders"
        }]
      }]
    },
    {
      "system" : "http://snomed.info/sct",
      "concept" : [{
        "extension" : [{
          "url" : "http://hl7.org/fhir/StructureDefinition/valueset-concept-comments",
          "valueString" : "Drinkt alcohol"
        }],
        "code" : "219006",
        "display" : "drinkt alcohol",
        "designation" : [{
          "language" : "en-US",
          "use" : {
            "system" : "http://snomed.info/sct",
            "code" : "900000000000013009",
            "display" : "Synonym"
          },
          "value" : "Current drinker"
        },
        {
          "language" : "nl-NL",
          "use" : {
            "system" : "http://snomed.info/sct",
            "code" : "900000000000013009",
            "display" : "Synonym"
          },
          "value" : "drinkt alcohol"
        }]
      },
      {
        "extension" : [{
          "url" : "http://hl7.org/fhir/StructureDefinition/valueset-concept-comments",
          "valueString" : "Niet-drinker"
        }],
        "code" : "105542008",
        "display" : "drinkt momenteel geen alcohol",
        "designation" : [{
          "language" : "en-US",
          "use" : {
            "system" : "http://snomed.info/sct",
            "code" : "900000000000013009",
            "display" : "Synonym"
          },
          "value" : "Non - drinker"
        },
        {
          "language" : "nl-NL",
          "use" : {
            "system" : "http://snomed.info/sct",
            "code" : "900000000000013009",
            "display" : "Synonym"
          },
          "value" : "drinkt momenteel geen alcohol"
        }]
      },
      {
        "extension" : [{
          "url" : "http://hl7.org/fhir/StructureDefinition/valueset-concept-comments",
          "valueString" : "Dronk vroeger alcohol"
        }],
        "code" : "82581004",
        "display" : "dronk vroeger alcohol",
        "designation" : [{
          "language" : "en-US",
          "use" : {
            "system" : "http://snomed.info/sct",
            "code" : "900000000000013009",
            "display" : "Synonym"
          },
          "value" : "Ex-drinker"
        },
        {
          "language" : "nl-NL",
          "use" : {
            "system" : "http://snomed.info/sct",
            "code" : "900000000000013009",
            "display" : "Synonym"
          },
          "value" : "dronk vroeger alcohol"
        }]
      },
      {
        "extension" : [{
          "url" : "http://hl7.org/fhir/StructureDefinition/valueset-deprecated",
          "valueBoolean" : true
        },
        {
          "url" : "http://hl7.org/fhir/StructureDefinition/valueset-concept-comments",
          "valueString" : "Levenslang niet drinker"
        }],
        "code" : "228274009",
        "display" : "levenslang niet-drinker",
        "designation" : [{
          "language" : "en-US",
          "use" : {
            "system" : "http://snomed.info/sct",
            "code" : "900000000000013009",
            "display" : "Synonym"
          },
          "value" : "Lifetime non-drinker"
        },
        {
          "language" : "nl-NL",
          "use" : {
            "system" : "http://snomed.info/sct",
            "code" : "900000000000013009",
            "display" : "Synonym"
          },
          "value" : "levenslang niet-drinker"
        }]
      },
      {
        "extension" : [{
          "url" : "http://hl7.org/fhir/StructureDefinition/valueset-concept-comments",
          "valueString" : "Levenslang niet drinker"
        }],
        "code" : "783261004",
        "display" : "levenslange niet-drinker van alcohol",
        "designation" : [{
          "language" : "en-US",
          "use" : {
            "system" : "http://snomed.info/sct",
            "code" : "900000000000013009",
            "display" : "Synonym"
          },
          "value" : "Lifetime non-drinker of alcohol"
        },
        {
          "language" : "nl-NL",
          "use" : {
            "system" : "http://snomed.info/sct",
            "code" : "900000000000013009",
            "display" : "Synonym"
          },
          "value" : "levenslange niet-drinker van alcohol"
        }]
      }]
    }]
  }
}

```
