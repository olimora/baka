CREATE OR REPLACE FUNCTION import_fve_csv(subor text)
RETURNS integer AS $$
BEGIN
------------------------------------------------------------------------------------------------------------------------------
--update cas na timestamp + vynulovat minuty
UPDATE t_produkcia_import i
	SET cas = to_timestamp(i.in_cas, 'DD.MM.YYYY HH24:00'); --upravit cas na UTC 
	--prec letny cas
	
------------------------------------------------------------------------------------------------------------------------------
--vymazat tie, pre ktore nie je predpoved
--DELETE 
SELECT * FROM t_produkcia_import WHERE cas NOT IN (SELECT cas FROM t_predpoved_hodina) order by cas, in_cas
------------------------------------------------------------------------------------------------------------------------------		
--update vykon 
UPDATE t_produkcia_import
	SET vykon = (select cast(in_vykon as real))
	WHERE in_vykon != '-';
--update na zmenu pomlcky na priemer predchadzajuceho a nasledujuceho udaju
UPDATE t_produkcia_import t1
	SET vykon = (select cast(avg(cast(in_vykon as real)) as real) from t_produkcia_import t2 where t2.id = t1.id - 1 or t2.id = t1.id + 1)
	WHERE t1.in_vykon = '-';
------------------------------------------------------------------------------------------------------------------------------
--insert into _den datum, vykon sum 24 hodin, fve_id
INSERT INTO t_produkcia_den (datum, vykon, fve) 
	(SELECT datum, vykon, fve FROM 
		(select to_date(to_char(cas, 'YYYY-MM-DD'),'YYYY-MM-DD') datum, sum(vykon) vykon, (SELECT id FROM t_fve WHERE nazov = in_fve) fve, in_fve	
			from t_produkcia_import
			group by in_fve, to_char(cas, 'YYYY-MM-DD')
			order by in_fve, to_char(cas, 'YYYY-MM-DD')) s1);
------------------------------------------------------------------------------------------------------------------------------
--insert into _hodina, vykon, den_id
INSERT INTO t_produkcia_hodina (cas, vykon, produkcia_den) 
	(SELECT cas, vykon, produkcia_den FROM
		(select cas, sum(vykon) vykon, (SELECT id FROM t_produkcia_den WHERE datum = to_date(to_char(cas, 'YYYY-MM-DD'), 'YYYY-MM-DD') 
			and fve = (SELECT id FROM t_fve WHERE nazov = in_fve)) produkcia_den, in_fve
			from t_produkcia_import
			group by in_fve, cas
			order by in_fve, cas) s1);
------------------------------------------------------------------------------------------------------------------------------
DELETE FROM t_produkcia_import;
RETURN 1;
------------------------------------------------------------------------------------------------------------------------------
END;
$$ LANGUAGE plpgsql

111330 + 111330 * 1160 = 129 254 130
111330 + 27832.5 + 27832.5 * 1160 = 32 424 862.5

select cast('132342' as double precision)