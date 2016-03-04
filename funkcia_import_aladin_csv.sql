CREATE OR REPLACE FUNCTION import_aladin_csv(subor text)
RETURNS integer AS 
$$
DECLARE
	zimny_slnovrat_2013 date;
	zimny_slnovrat_2014 date;
	zimny_slnovrat_2015 date;
	zimny_slnovrat_2016 date; 
	lokalita_id integer;
BEGIN
-------------------------------------------------------------------------------------------------------
zimny_slnovrat_2013 = to_date('2013-12-21','YYYY-MM-DD');
zimny_slnovrat_2014 = to_date('2014-12-22','YYYY-MM-DD');
zimny_slnovrat_2015 = to_date('2015-12-22','YYYY-MM-DD');
zimny_slnovrat_2016 = to_date('2016-12-21','YYYY-MM-DD');
-------------------------------------------------------------------------------------------------------
SET TIMEZONE = 'UTC';
-------------------------------------------------------------------------------------------------------
DELETE FROM t_predpoved_import;
-------------------------------------------------------------------------------------------------------
EXECUTE format('COPY t_predpoved_import FROM %L WITH DELIMITER ' || quote_literal(',') || ';', subor);
-------------------------------------------------------------------------------------------------------
INSERT INTO t_predpoved_hodina (cas, gho, oblacnost, teplota, rychlost_vetra, vlhkost, tlak) 
	(SELECT to_timestamp(datum || ' ' || cas, 'DD-MM-YYYY HH24:MI') cas, gho, oblacnost, 
			teplota, rychlost_vetra, vlhkost, tlak 
		FROM t_predpoved_import); 
		--WHERE gho > 0); -- pozriet ktore mozem vymazat z gho 0 a fve 0
-------------------------------------------------------------------------------------------------------
IF subor like '%DUB%' THEN 
	SELECT id INTO lokalita_id FROM t_lokalita WHERE nazov = 'Dubravy';
ELSIF subor like '%PLE%' THEN
	SELECT id INTO lokalita_id FROM t_lokalita WHERE nazov = 'Plesivec';
ELSE RETURN -1;	
END IF;
-------------------------------------------------------------------------------------------------------
INSERT INTO t_predpoved_den (lokalita, datum)
	(SELECT DISTINCT lokalita_id lokalita, 
		to_date(to_char(cas, 'YYYY-MM-DD'),'YYYY-MM-DD') datum 
		FROM t_predpoved_hodina WHERE predpoved_den IS NULL --zrejme netreba
		ORDER BY datum);

UPDATE t_predpoved_hodina h 
	SET predpoved_den = (SELECT d.id 
				FROM t_predpoved_den d 
				WHERE to_char(d.datum, 'YYYY-MM-DD') = to_char(h.cas, 'YYYY-MM-DD')
				AND d.lokalita = lokalita_id) 
	WHERE h.predpoved_den IS NULL;	
-------------------------------------------------------------------------------------------------------
UPDATE t_predpoved_den t
	SET sklon = (select MIN(sklon) from (
			(select abs(zimny_slnovrat_2013 - t.datum) sklon) 
			UNION
			(select abs(zimny_slnovrat_2014 - t.datum) sklon) 
			UNION 
			(select abs(zimny_slnovrat_2015 - t.datum) sklon) 
			UNION 
			(select abs(zimny_slnovrat_2016 - t.datum) sklon)
					) s1) + 1
	WHERE t.sklon IS NULL;	
-------------------------------------------------------------------------------------------------------
DELETE FROM t_predpoved_import;
RETURN 1;
-------------------------------------------------------------------------------------------------------
END;
$$ 
LANGUAGE plpgsql