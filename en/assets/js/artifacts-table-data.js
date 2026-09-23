window.artifactsTableData = {
  "en": {
    "labels": {
      "type":        "Type",
      "category":    "Category",
      "useGrouping": "Use grouping",
      "clearAll":    "Clear all"
    },
    "groupDescriptions": {
      "-dyn-capabilitystatement": "<p>The following artifacts define the specific capabilities that different types of systems are expected to have in order to comply with this implementation guide.  Systems conforming to this implementation guide are expected to declare conformance to one or more of the following capability statements.</p>\n"
      ,"-str-logicalmodel": "<p>These define data models that represent the domain covered by this implementation guide in more business-friendly terms than the underlying FHIR resources.</p>\n"
      ,"-str-profile": "<p>These define constraints on FHIR resources for systems conforming to this implementation guide.</p>\n"
      ,"-term-valueset": "<p>These define sets of codes used by systems conforming to this implementation guide.</p>\n"
      ,"-term-codesystem": "<p>These define new code systems used by systems conforming to this implementation guide.</p>\n"
      ,"-ex-example": "<p>These are example instances that show what data produced and consumed by systems conforming with this implementation guide might look like.</p>\n"
      ,"-other": "<p>These are resources that are used within this implementation guide that do not fit into one of the other categories.</p>\n"
    },
    "rows": [
      { "p":1, "gid":"-dyn-capabilitystatement", "g":"Behavior: Capability Statements", "n":"Santeon CarePlan Server Capability Statement", "i":"SanteonCarePlanCapabilityStatement", "t":"CapabilityStatement", "u":"CapabilityStatement-SanteonCarePlanCapabilityStatement.html", "r":"CapabilityStatement/SanteonCarePlanCapabilityStatement", "d":"<p>Describes the FHIR server capabilities for the Santeon CarePlan IG.</p>" },
      { "p":2, "gid":"-str-logicalmodel", "g":"Structures: Logical Models", "n":"De-identification Ruleset", "i":"deidentification-ruleset", "t":"StructureDefinition", "u":"StructureDefinition-deidentification-ruleset.html", "r":"StructureDefinition/deidentification-ruleset", "d":"<p>A fully-resolved (effective) set of de-identification rules for one export.\nIt carries the actual rules in force, with no reference to a base ruleset\nplus differences. Together with the export Parameters (which declare the\nreleased elements via _elements), it fully describes how an export is\nde-identified.</p>" },
      { "p":3, "gid":"-str-profile", "g":"Structures: Resource Profiles", "n":"Santeon CarePlan", "i":"santeon-careplan", "t":"StructureDefinition", "u":"StructureDefinition-santeon-careplan.html", "r":"StructureDefinition/santeon-careplan", "d":"<p>Santeon-internal CarePlan profile derived from base FHIR R4.\nOnly the elements present in the Santeon data dictionary are\nexposed; all other optional base elements are prohibited (0..0).</p>\n\n<p>De-identification strategy per element:</p>\n<ul>\n  <li>subject.identifier → hash</li>\n  <li>period.start       → dateshift</li>\n  <li>period.end         → dateshift</li>\n</ul>" },
      { "p":4, "gid":"-term-valueset", "g":"Terminology: Value Sets", "n":"De-identification Action Value Set", "i":"deidentification-action-vs", "t":"ValueSet", "u":"ValueSet-deidentification-action-vs.html", "r":"ValueSet/deidentification-action-vs", "d":"<p>Allowed values for a de-identification rule's action.</p>" },
      { "p":5, "gid":"-term-codesystem", "g":"Terminology: Code Systems", "n":"De-identification Action Code System", "i":"deidentification-action-cs", "t":"CodeSystem", "u":"CodeSystem-deidentification-action-cs.html", "r":"CodeSystem/deidentification-action-cs", "d":"<p>The transformations a de-identification rule may apply to an element.</p>" },
      { "p":6, "gid":"-ex-example", "g":"Example: Example Instances", "n":"Voorbeeld Patiënt", "i":"ExamplePatient", "t":"Patient", "u":"Patient-ExamplePatient.html", "r":"Patient/ExamplePatient", "d":"<p>Minimale Patient resource voor gebruik in het CarePlan voorbeeld.</p>" },
      { "p":6, "gid":"-ex-example", "g":"Example: Example Instances", "n":"Voorbeeld Santeon CarePlan (CKD conservatief)", "i":"SanteonCarePlanExample", "t":"CarePlan", "u":"CarePlan-SanteonCarePlanExample.html", "r":"CarePlan/SanteonCarePlanExample", "d":"<p>Minimaal CarePlan conform het SanteonCarePlan profiel. CKD-scenario: conservatief traject voorgesteld.</p>" },
      { "p":7, "gid":"-other", "g":"Other", "n":"Santeon Standard De-identification Ruleset", "i":"santeon-default", "t":"Binary", "u":"Binary-santeon-default.html", "r":"Binary/santeon-default", "d":"<p>The default ruleset every Santeon export inherits unless overridden.</p>" }
    ]
  }
};
