// ============================================================
// CapabilityStatement — Santeon CarePlan Server
// ============================================================

Instance: SanteonCarePlanCapabilityStatement
InstanceOf: CapabilityStatement
Title: "Santeon CarePlan Server Capability Statement"
Description: "Describes the FHIR server capabilities for the Santeon CarePlan IG."
Usage: #definition

* name   = "SanteonCarePlanCapabilityStatement"
* status = #draft
* experimental = true
* date   = "2025-01-01"
* kind   = #requirements
* fhirVersion = #4.0.1
* format[+] = #json
* format[+] = #xml

* rest[+].mode = #server
* rest[=].resource[+].type = #CarePlan
* rest[=].resource[=].profile = "https://ig.santeon.nl/careplan/StructureDefinition/santeon-careplan"
* rest[=].resource[=].interaction[+].code = #read
* rest[=].resource[=].interaction[+].code = #search-type
* rest[=].resource[=].interaction[+].code = #create
* rest[=].resource[=].interaction[+].code = #update
* rest[=].resource[=].searchParam[+].name   = "status"
* rest[=].resource[=].searchParam[=].type   = #token
* rest[=].resource[=].searchParam[=].definition = "http://hl7.org/fhir/SearchParameter/CarePlan-status"
* rest[=].resource[=].searchParam[+].name   = "category"
* rest[=].resource[=].searchParam[=].type   = #token
* rest[=].resource[=].searchParam[=].definition = "http://hl7.org/fhir/SearchParameter/CarePlan-category"
* rest[=].resource[=].searchParam[+].name   = "patient"
* rest[=].resource[=].searchParam[=].type   = #reference
* rest[=].resource[=].searchParam[=].definition = "http://hl7.org/fhir/SearchParameter/clinical-patient"
* rest[=].resource[=].searchParam[+].name   = "date"
* rest[=].resource[=].searchParam[=].type   = #date
* rest[=].resource[=].searchParam[=].definition = "http://hl7.org/fhir/SearchParameter/clinical-date"
