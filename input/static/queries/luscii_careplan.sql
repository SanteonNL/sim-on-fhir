SELECT DISTINCT
    -- Composite identifier from patient number, program, and enrollment date
    COALESCE(t1.Patient_Number, '') || '-' || COALESCE(t3.name, '') || '-' || COALESCE(t2.createdAt, '') AS careplanidentifier,
    -- Fixed empty system, code comes from program
    '' AS careplancategorycodingsystem,
    -- Program name serves as the category code
    t3.name AS careplancategorycodingcode,
    -- Program enrollment date formatted as ISO 8601 UTC
    strftime('%Y-%m-%dT%H:%M:%SZ', t2.createdAt) AS careplanperiodstart,
    strftime('%Y-%m-%dT%H:%M:%SZ',
        (
            SELECT MIN(uscr.processedAt)
            FROM luscii_usersStatusChangeReasons uscr
            WHERE uscr.userId = t1.Patient_UUID
              AND uscr.toStatus = 'stopped'
              AND uscr.processedAt > t2.createdAt
              AND uscr.processedAt < COALESCE(
                  (SELECT MIN(pph2.createdAt)
                   FROM luscii_patientsprogramshistory pph2
                   WHERE pph2.patientId = t1.Patient_UUID
                     AND pph2.programId = t2.programId
                     AND pph2.createdAt > t2.createdAt),
                  '9999-12-31')
        )
    -- Finds the earliest 'stopped' status change after program enrollment, but not beyond the next program enrollment.
    -- Returns NULL if no stop event exists (care is ongoing). Temporal constraints prevent overlapping periods.
    ) AS careplanperiodend 
FROM luscii_patients t1
LEFT JOIN luscii_patientsprogramshistory t2 ON t1.Patient_UUID = t2.patientId
LEFT JOIN luscii_programs t3 ON t2.programId = t3.id
