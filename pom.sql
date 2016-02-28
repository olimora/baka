select * from t_predpoved_import

DELETE FROM t_predpoved_import;
DELETE FROM t_predpoved_hodina;
DELETE FROM t_predpoved_den;

COPY t_predpoved_import FROM 'C:\000\skola\baka\data\Aladin\gho\2014-08\ALADIN-DUB_48.587_19.369-2014-08.csv' WITH DELIMITER ','

INSERT INTO t_predpoved_hodina (cas, gho, oblacnost, teplota, rychlost_vetra, vlhkost, tlak) 
	(SELECT to_timestamp(datum || ' ' || cas, 'DD-MM-YYYY HH24:MI') cas, gho, oblacnost, teplota, rychlost_vetra, vlhkost, tlak FROM t_predpoved_import WHERE gho > 0)

INSERT INTO t_predpoved_den (lokalita, datum)
	(SELECT DISTINCT 	(select id from t_lokalita where nazov = 'Dubravy') lokalita, 
				to_date(to_char(cas, 'YYYY-MM-DD'),'YYYY-MM-DD') datum 
		FROM t_predpoved_hodina WHERE predpoved_den IS NULL ORDER BY datum 	)

UPDATE t_predpoved_hodina h SET predpoved_den = (SELECT d.id FROM t_predpoved_den d WHERE to_char(d.datum, 'YYYY-MM-DD') = to_char(h.cas, 'YYYY-MM-DD')) WHERE h.predpoved_den IS NULL

select * from t_lokalita l, t_predpoved_den d, t_predpoved_hodina h where l.id = d.lokalita and d.id = h.predpoved_den order by h.cas, l.nazov ASC
select * from t_predpoved_den

DROP FUNCTION import_aladin_csv(text)

Drop table t_produkcia_import;
DELETE FROM t_produkcia_import;
select * from t_produkcia_import order by id
select * from t_predpoved_hodina
select id, in_cas, in_vykon from t_produkcia_import where in_vykon = '-'
select (select cast(vykon as real) vykonn) from t_produkcia_import where in_vykon != '-'
select to_char((select cast(vykon as real) vykonn), '000.9') vykonnn from t_produkcia_import where in_vykon != '-'

select * from t_produkcia_import s1 where in_vykon = '-'

select id, 
	(select in_vykon from t_produkcia_import s2 where s2.id = s1.id - 1) vykon2,
	(select in_vykon from t_produkcia_import s3 where s3.id = s1.id + 1) vykon3
from t_produkcia_import s1 where in_vykon = '-'

select (select cast(avg(cast(in_vykon as real)) as real) from t_produkcia_import s2 where s2.id = s1.id - 1 or s2.id = s1.id + 1) vykon2
from t_produkcia_import s1 where in_vykon = '-'




SELECT in_fve, cas, sum(vykonnn) from 
	(select in_fve, id, in_cas, cas, (select cast(sin_vykon as real)) vykonnn from t_produkcia_import where in_vykon != '-') t
	group by in_fve, cas
	order by in_fve, cas

select * from 
(SELECT id, in_fve, cas, in_vykon from t_produkcia_import) s1,
(SELECT  sum(vykonnn), cas, in_fve from 
	(select in_fve, id, in_cas, cas, (select cast(in_vykon as real)) vykonnn from t_produkcia_import where in_vykon != '-') t
	group by in_fve, cas order by in_fve, cas) s2
where s1.cas = s2.cas and s1.in_fve = s2.in_fve
order by id


UPDATE t_produkcia_import i
	SET cas = to_timestamp(i.in_cas, 'DD.MM.YYYY HH24:00')

select t1.cas, t1.gho, t2.vykon, t2.cas 
from t_predpoved_hodina t1, t_produkcia_hodina t2--, t_predpoved_den t3, t_produkcia_den t4 
where t1.cas = t2.cas 
and t1.predpoved_den = t3.id

select * from t_lokalita
select * from t_fve

SELECT * FROM
(select t2.lokalita s1_lokalita, t1.cas s1_cas, t1.gho from t_predpoved_hodina t1, t_predpoved_den t2 where t1.predpoved_den = t2.id and t2.lokalita = 1) s1,
(select t1.vykon, t1.cas s2_cas from t_produkcia_hodina t1, t_produkcia_den t2 where t1.produkcia_den = t2.id and t2.fve =  1) s2
WHERE s1_cas = s2_cas
ORDER BY s1_cas
