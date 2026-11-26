-- HIPS AlgemeneMeting — Observation extract for IBD indicator K3.6.3
-- Source: AlgemeneMeting table in the HIPS data model (ZIB AlgemeneMeting-v3.0)

SELECT DISTINCT
    -- Patient identifier (BSN), de-identification: hash
    am.Identificatienummer
    AS observationsubjectidentifier,
    -- Measurement date/time (ISO 8601), de-identification: dateshift
    am.MetingDatumTijd
    AS observationeffectivedatetime,
    -- LOINC code for measurement type (e.g. 38445-3 = Calprotectin)
    am.MetingNaamCode
    AS observationcodingcode
FROM AlgemeneMeting am
