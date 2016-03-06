CREATE OR REPLACE FUNCTION delete_abundant_data()
RETURNS integer AS 
$$
BEGIN
-------------------------------------------------------------------------------------------------------
-- odstranit chybne - tie kde chybal vykon 
DELETE FROM t_produkcia_hodina WHERE vykon > 2000;
-- tie kde chybaju zaznamy
DELETE FROM t_produkcia_hodina WHERE id IN 
	(select hod.id from t_produkcia_den den, t_produkcia_hodina hod 
		where hod.produkcia_den = den.id 
		and den.fve = (SELECT id FROM t_fve WHERE nazov like '%Plesivec%')
		and (den.datum = to_date('2015-10-28','YYYY-MM-DD') 
			or (den.datum >= to_date('2015-7-25','YYYY-MM-DD')
				and den.datum <= to_date('2015-7-31','YYYY-MM-DD'))));
-------------------------------------------------------------------------------------------------------
-- odstranit tie, kde je gho aj vykon 0
DELETE FROM t_produkcia_hodina WHERE id IN 
	(SELECT t1.id FROM t_produkcia_hodina t1, t_predpoved_hodina t2 
		WHERE t1.predpoved = t2.id and t1.vykon = 0 and t2.gho = 0);
-- odstranit den ku kazdej odstranenej hodine
DELETE FROM t_produkcia_den WHERE id NOT IN (SELECT produkcia_den FROM t_produkcia_hodina);
-------------------------------------------------------------------------------------------------------
-- odstranit tie, co nemaju par
DELETE FROM t_produkcia_hodina WHERE predpoved IS NULL;
DELETE FROM t_produkcia_den WHERE predpoved IS NULL;
DELETE FROM t_predpoved_hodina WHERE id NOT IN (SELECT predpoved FROM t_produkcia_hodina);
DELETE FROM t_predpoved_den WHERE id NOT IN (SELECT predpoved FROM t_produkcia_den);
-------------------------------------------------------------------------------------------------------
RETURN 1;
-------------------------------------------------------------------------------------------------------
END;
$$ 
LANGUAGE plpgsql