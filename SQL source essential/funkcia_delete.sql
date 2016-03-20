CREATE OR REPLACE FUNCTION delete_abundant_data()
RETURNS integer AS
$$
DECLARE
	plesivec_id integer;
BEGIN
-------------------------------------------------------------------------------------------------------
-- odstranit chybne - tie kde chybal vykon
DELETE FROM t_produkcia_hodina WHERE produkcia_den IN
	(select t2.id from t_produkcia_hodina t1, t_produkcia_den t2
    	where t1.produkcia_den = t2.id and t1.vykon > 2000);
-- tie kde chybaju zaznamy
SELECT id INTO plesivec_id FROM t_fve WHERE nazov like '%Plesivec%';
DELETE FROM t_produkcia_hodina WHERE produkcia_den IN
	(select id from t_produkcia_den
		where fve = plesivec_id
		and (datum = to_date('2015-10-28','YYYY-MM-DD')
			or (datum >= to_date('2015-7-25','YYYY-MM-DD')
				and datum <= to_date('2015-7-31','YYYY-MM-DD'))));
-- odstranit tie, kde je gho aj vykon aj praca 0
DELETE FROM t_produkcia_hodina WHERE id IN
	(select t1.id from t_produkcia_hodina t1, t_predpoved_hodina t2
		where t1.predpoved = t2.id and t1.vykon = 0 and t1.praca = 0 and t2.gho = 0);
-- odstranit dni, pre ktore nemame hodiny
DELETE FROM t_produkcia_den WHERE id NOT IN (SELECT produkcia_den FROM t_produkcia_hodina);
-------------------------------------------------------------------------------------------------------
-- odstranit tie, co nemaju par
DELETE FROM t_produkcia_hodina WHERE predpoved IS NULL;
DELETE FROM t_produkcia_den WHERE predpoved IS NULL;
DELETE FROM t_predpoved_hodina WHERE id NOT IN (SELECT predpoved FROM t_produkcia_hodina);
DELETE FROM t_predpoved_den WHERE id NOT IN (SELECT predpoved FROM t_produkcia_den);
-------------------------------------------------------------------------------------------------------
DELETE FROM t_solar_den WHERE datum NOT IN (SELECT datum FROM t_predpoved_den);
DELETE FROM t_solar_hod WHERE cas NOT IN (SELECT cas FROM t_predpoved_hodina);
RETURN 1;
-------------------------------------------------------------------------------------------------------
END;
$$
LANGUAGE plpgsql