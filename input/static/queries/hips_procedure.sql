-- HIPS Verrichting — Procedure extract for IBD indicator K3.6.3
-- Source: Verrichting table in the HIPS data model (ZIB Verrichting-v5.1)

SELECT DISTINCT
    -- Patient identifier (BSN), de-identification: hash
    v.Identificatienummer
    AS proceduresubjectidentifier,
    -- Procedure start date (ISO 8601), de-identification: dateshift
    v.VerrichtingStartDatum
    AS procedureperformedstart,
    -- NZa verrichtingencode (e.g. 034620, 034686, 034690, 035582 for scopie)
    v.VerrichtingTypeCodeNZa
    AS procedurecodingnza,
    -- SNOMED CT procedure code
    v.VerrichtingTypeCodeSnomedCT
    AS procedurecodingsnomed,
    -- DHD verrichtingencode
    v.VerrichtingTypeCodeDHD
    AS procedurecodingdhd,
    -- Number of procedures performed (no standard FHIR mapping)
    v.VerrichtingAantal
    AS procedureamount,
    -- Performer specialty (AGB code)
    v.Uitvoerder_Specialisme
    AS procedureperformerspecialty,
    -- Requester specialty (AGB code)
    v.Aanvrager_Specialisme
    AS procedurerequesterspecialty
FROM Verrichting v
