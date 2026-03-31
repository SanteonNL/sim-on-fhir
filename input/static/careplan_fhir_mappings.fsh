// ============================================================
// FHIR Mappings - SQL Tables & Columns
// Source: careplan.sql
// ============================================================

* ^mapping[+].identity = "careplan.sql"
* ^mapping[=].name = "SQL Query Mapping"
* ^mapping[=].uri = "urn:santeon:mapping:careplan.sql"
* ^mapping[=].comment = "Tables and columns from careplan.sql"

* identifier ^mapping[0].identity = "careplan.sql"
* identifier ^mapping[0].map = "luscii_patients.Patient_Number, luscii_programs.name, luscii_patientsprogramshistory.createdAt"
* identifier ^mapping[0].comment = "Columns: luscii_patients.Patient_Number, luscii_programs.name, luscii_patientsprogramshistory.createdAt"

* category ^mapping[0].identity = "careplan.sql"
* category ^mapping[0].map = "luscii_programs.name"
* category ^mapping[0].comment = "Columns: luscii_programs.name"

* category.coding.code ^mapping[0].identity = "careplan.sql"
* category.coding.code ^mapping[0].map = "luscii_programs.name"
* category.coding.code ^mapping[0].comment = "Columns: luscii_programs.name"

* category.coding.system ^mapping[0].identity = "careplan.sql"
* category.coding.system ^mapping[0].map = "(profile fixed value)"
* category.coding.system ^mapping[0].comment = "Not from database"

* subject ^mapping[0].identity = "careplan.sql"
* subject ^mapping[0].map = "luscii_patients.Patient_UUID, luscii_patients.Patient_Number"
* subject ^mapping[0].comment = "Columns: luscii_patients.Patient_UUID, luscii_patients.Patient_Number"

* period ^mapping[0].identity = "careplan.sql"
* period ^mapping[0].map = "luscii_patientsprogramshistory.createdAt, luscii_usersStatusChangeReasons.processedAt"
* period ^mapping[0].comment = "Columns: luscii_patientsprogramshistory.createdAt, luscii_usersStatusChangeReasons.processedAt"

* period.start ^mapping[0].identity = "careplan.sql"
* period.start ^mapping[0].map = "luscii_patientsprogramshistory.createdAt"
* period.start ^mapping[0].comment = "Columns: luscii_patientsprogramshistory.createdAt"

* period.end ^mapping[0].identity = "careplan.sql"
* period.end ^mapping[0].map = "luscii_usersStatusChangeReasons.processedAt, luscii_patientsprogramshistory.createdAt"
* period.end ^mapping[0].comment = "Columns: luscii_usersStatusChangeReasons.processedAt, luscii_patientsprogramshistory.createdAt"
