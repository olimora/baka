CREATE OR REPLACE FUNCTION import_aladin_csv(subor text)
RETURNS integer AS $$
BEGIN
-------------------------------------------------------------------------------------------------------
DELETE FROM t_predpoved_import;
-------------------------------------------------------------------------------------------------------
EXECUTE format('COPY t_predpoved_import FROM %L WITH DELIMITER ' || quote_literal(',') || ';', subor);
-------------------------------------------------------------------------------------------------------
INSERT INTO t_predpoved_hodina (cas, gho, oblacnost, teplota, rychlost_vetra, vlhkost, tlak) 
	(SELECT to_timestamp(datum || ' ' || cas, 'DD-MM-YYYY HH24:MI') cas, gho, oblacnost, 
			teplota, rychlost_vetra, vlhkost, tlak 
		FROM t_predpoved_import 
		WHERE gho > 0);
-------------------------------------------------------------------------------------------------------
IF subor like '%DUB%' THEN 
INSERT INTO t_predpoved_den (lokalita, datum)
	(SELECT DISTINCT (select id from t_lokalita where nazov = 'Dubravy') lokalita, 
		to_date(to_char(cas, 'YYYY-MM-DD'),'YYYY-MM-DD') datum 
		FROM t_predpoved_hodina WHERE predpoved_den IS NULL 
		ORDER BY datum);

UPDATE t_predpoved_hodina h 
	SET predpoved_den = (SELECT d.id 
				FROM t_predpoved_den d 
				WHERE to_char(d.datum, 'YYYY-MM-DD') = to_char(h.cas, 'YYYY-MM-DD')
				AND d.lokalita = (select id from t_lokalita where nazov = 'Dubravy')) 
	WHERE h.predpoved_den IS NULL;
		
ELSE 
INSERT INTO t_predpoved_den (lokalita, datum)
	(SELECT DISTINCT (select id from t_lokalita where nazov = 'Plesivec') lokalita, 
		to_date(to_char(cas, 'YYYY-MM-DD'),'YYYY-MM-DD') datum 
		FROM t_predpoved_hodina WHERE predpoved_den IS NULL 
		ORDER BY datum);

UPDATE t_predpoved_hodina h 
	SET predpoved_den = (SELECT d.id 
				FROM t_predpoved_den d 
				WHERE to_char(d.datum, 'YYYY-MM-DD') = to_char(h.cas, 'YYYY-MM-DD')
				AND d.lokalita = (select id from t_lokalita where nazov = 'Plesivec')) 
	WHERE h.predpoved_den IS NULL;

END IF;
DELETE FROM t_predpoved_import;
RETURN 1;
-------------------------------------------------------------------------------------------------------
END;
$$ LANGUAGE plpgsql