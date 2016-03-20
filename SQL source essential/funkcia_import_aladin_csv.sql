CREATE OR REPLACE FUNCTION import_aladin_csv(subor text)
RETURNS integer AS
$$
DECLARE
	lokalita_id integer;
BEGIN
---------------------------------------------------------------------------
SET TIMEZONE = 'UTC';
DELETE FROM t_predpoved_import;
EXECUTE format('COPY t_predpoved_import FROM %L WITH DELIMITER '
					|| quote_literal(',') || ';', subor);
---------------------------------------------------------------------------
INSERT INTO t_predpoved_hodina (cas, gho, oblacnost, teplota,
	vietor, vlhkost, tlak)
	(SELECT to_timestamp(datum || ' ' || cas, 'DD-MM-YYYY HH24:MI') cas,
    		gho, oblacnost, teplota, vietor, vlhkost, tlak
		FROM t_predpoved_import);
---------------------------------------------------------------------------
IF subor like '%DUB%' THEN
	SELECT id INTO lokalita_id FROM t_lokalita WHERE nazov = 'Dubravy';
ELSIF subor like '%PLE%' THEN
	SELECT id INTO lokalita_id FROM t_lokalita WHERE nazov = 'Plesivec';
ELSE RETURN -1;
END IF;
---------------------------------------------------------------------------
INSERT INTO t_predpoved_den (lokalita, datum)
	(SELECT DISTINCT lokalita_id lokalita,
		to_date(to_char(cas, 'YYYY-MM-DD'),'YYYY-MM-DD') datum
		FROM t_predpoved_hodina WHERE predpoved_den IS NULL
		ORDER BY datum);
UPDATE t_predpoved_hodina h
	SET predpoved_den = (SELECT d.id FROM t_predpoved_den d
		WHERE to_char(d.datum, 'YYYY-MM-DD') = to_char(h.cas, 'YYYY-MM-DD')
		AND d.lokalita = lokalita_id)
	WHERE h.predpoved_den IS NULL;
---------------------------------------------------------------------------
DELETE FROM t_predpoved_import;
RETURN 1;
---------------------------------------------------------------------------
END;
$$
LANGUAGE plpgsql