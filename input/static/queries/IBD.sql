/*
Alle patiënten met:
    - CarePlanCategoryCodingCode ZBJ_IBD gestart op of na 01-01-2018. (Let op, niet dynamisch!); of
    - één van de volgende DOTs:
        - 0313 (Interne Geneeskunde) met diagnosecode 922, 923 indien een internist werkzaam is met specialisatie maag-darm-leverziekten; of
        - 0318 (MDL)                 met diagnosecode 601, 602;
      De DOT moet zorgtype 11 of 21 hebben en geopend zijn op of na 01-01-2018. (Let op, niet dynamisch!)
Patiënten jonger dan 18 jaar op het moment van aanleveren worden geëxcludeerd.
*/

/*prescript*/
SELECT
    DISTINCT identificatienummer = e.PATIENTNR
FROM
    EPISODE_DBCPER d
    JOIN EPISODE_EPISODE e ON d.EPISODE = e.EPISODE
    JOIN CSZISLIB_SPEC s ON d.SPECIALISM = s.SPECCODE
    JOIN EPISODE_ZORGTYPE z ON d.ZORGTYPE = z.CODE
    JOIN PATIENT_PATIENT p ON e.PATIENTNR = p.PATIENTNR
/*joinstatement*/
WHERE 1=1
    AND d.BEGINDAT >= '2018-01-01'
    AND p.GEBDAT < DATEADD(year, -18, GETDATE())
    AND z.LANDELIJK IN ('11','21')
    AND d.VERVALLEN = 0
    AND (
            (
                CONCAT(s.ZORGVSOORT, FORMAT(s.COTGCODE, '00')) = '0313' AND d.HOOFDDIAG IN ('922','923')
            )
            OR
            (
                CONCAT(s.ZORGVSOORT, FORMAT(s.COTGCODE, '00')) = '0318' AND d.HOOFDDIAG IN ('601','602')
            )
    )