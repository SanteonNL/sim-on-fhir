-- Patient Journey system — CarePlan extract
-- Source: pj_* tables in the PatientJourney data warehouse

SELECT DISTINCT
    pat.patient_ref || '-' || prog.programme_code || '-' || enr.enrolment_date AS careplanidentifier,
    'https://patientjourney.nl/fhir/programme' AS careplancategorycodingsystem,
    prog.programme_code AS careplancategorycodingcode,
    strftime('%Y-%m-%dT%H:%M:%SZ', enr.enrolment_date) AS careplanperiodstart,
    strftime('%Y-%m-%dT%H:%M:%SZ', enr.discharge_date) AS careplanperiodend
FROM pj_patients pat
LEFT JOIN pj_enrolments enr ON pat.patient_ref = enr.patient_ref
LEFT JOIN pj_programmes prog ON enr.programme_id = prog.id
