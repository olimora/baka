DELETE FROM t_predpoved_import;

COPY t_predpoved_import FROM 'C:\\000\\skola\\baka\data\Aladin\gho\2014-08\ALADIN-PLE_48.544_20.400-2014-08.csv' WITH DELIMITER ',';

INSERT INTO t_predpoved_hodina (cas, gho, oblacnost, teplota, rychlost_vetra, vlhkost, tlak) 
	(SELECT to_timestamp(datum || ' ' || cas, 'DD-MM-YYYY HH24:MI') cas, gho, oblacnost, teplota, rychlost_vetra, vlhkost, tlak 
		FROM t_predpoved_import 
		WHERE gho > 0);

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


SELECT import_aladin_csv('C:\\000\skola\baka\data\Aladin\gho\2014-10\ALADIN-PLE_48.544_20.400-2014-10.csv');

select datum, lokalita from t_predpoved_den group by datum, lokalita order by datum desc