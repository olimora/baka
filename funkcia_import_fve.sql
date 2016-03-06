CREATE OR REPLACE FUNCTION import_fve_csv()
RETURNS integer AS 
$$
BEGIN
------------------------------------------------------------------------------------------------------------------------------
SET timezone='CET';
--update cas na timestamp v UTC
UPDATE t_produkcia_import t
	SET cas_cet = to_timestamp(t.in_cas, 'DD.MM.YYYY HH24:MI'); 
SET timezone='UTC';
UPDATE t_produkcia_import t
	SET cas = cas_cet at time zone 'UTC'; 
UPDATE t_produkcia_import t
	SET group_by_cas = to_timestamp(to_char(cas, 'YYYY-MM-DD HH24:00'), 'YYYY-MM-DD HH24:MI')
	WHERE cast(to_char(cas, 'MI') as integer) <= 30; 
UPDATE t_produkcia_import t
	SET group_by_cas = to_timestamp(to_char(cas, 'YYYY-MM-DD HH24:00'), 'YYYY-MM-DD HH24:MI') + '1 hours'
	WHERE cast(to_char(cas, 'MI') as integer) > 30; 
------------------------------------------------------------------------------------------------------------------------------
--update vykon 
UPDATE t_produkcia_import
	SET vykon = (select cast(in_vykon as real))
	WHERE in_vykon != '-';
--update na zmenu pomlcky na priemer predchadzajuceho a nasledujuceho udaju
UPDATE t_produkcia_import t1
	SET vykon = (select cast(avg(cast(in_vykon as real)) as real) 
			from t_produkcia_import t2 where t2.id = t1.id - 1 or t2.id = t1.id + 1)
	WHERE t1.in_vykon = '-';
------------------------------------------------------------------------------------------------------------------------------
--insert into _den datum, vykon sum 24 hodin, fve_id
INSERT INTO t_produkcia_den (datum, vykon, fve) 
	(SELECT datum, vykon, fve FROM 
		(select to_date(to_char(cas, 'YYYY-MM-DD'),'YYYY-MM-DD') datum, sum(vykon) vykon, 
			(SELECT id FROM t_fve WHERE nazov = in_fve) fve, in_fve	
			from t_produkcia_import
			group by in_fve, to_char(cas, 'YYYY-MM-DD')
			order by in_fve, to_char(cas, 'YYYY-MM-DD')) s1);
------------------------------------------------------------------------------------------------------------------------------
--insert into _hodina, vykon, den_id
INSERT INTO t_produkcia_hodina (cas, vykon, produkcia_den) 
	(SELECT cas, vykon, produkcia_den FROM
		(select group_by_cas cas, sum(vykon) vykon, in_fve, group_by_cas,
			(SELECT id FROM t_produkcia_den 
				WHERE datum = to_date(to_char(group_by_cas, 'YYYY-MM-DD'), 'YYYY-MM-DD')
				and fve = (SELECT id FROM t_fve WHERE nazov = in_fve)) produkcia_den
			from t_produkcia_import
			group by in_fve, group_by_cas
			order by in_fve, group_by_cas) s1);
------------------------------------------------------------------------------------------------------------------------------
--naparovat produkciu na predpovede
UPDATE t_produkcia_den t1
	SET predpoved = (select t2.id from t_predpoved_den t2, t_fve t3 
				where t1.fve = t3.id and t2.lokalita = t3.lokalita and t2.datum = t1.datum);
UPDATE t_produkcia_hodina t1
	SET predpoved = (select t2.id from t_predpoved_hodina t2, t_produkcia_den t3, t_predpoved_den t4 
				where t1.produkcia_den = t3.id and t2.predpoved_den = t4.id 
				and t3.predpoved = t4.id and t1.cas = t2.cas);
------------------------------------------------------------------------------------------------------------------------------
DELETE FROM t_produkcia_import;
------------------------------------------------------------------------------------------------------------------------------
RETURN 1;
------------------------------------------------------------------------------------------------------------------------------
END;
$$ 
LANGUAGE plpgsql