CREATE TABLE t_lokalita (
	id	serial 	primary key,
	nazov	varchar(40) NOT NULL
);

DROP TABLE t_produkcia;

INSERT INTO t_lokalita VALUES
	(DEFAULT, 'Dubravy'),
	(DEFAULT, 'Plesivec');

SELECT * FROM t_fve f, t_lokalita l WHERE f.lokalita = l.id


CREATE TABLE t_fve (
	id 	serial	primary key,
	nazov	varchar(40) NOT NULL,
	instalovany_vykon	real,
	rocny_predpoklad	real,
	lokalita	integer	references t_lokalita(id)
);

INSERT INTO t_fve VALUES
	(DEFAULT, 'FVE Dubravy 1', 0.850, 901.641, (SELECT id FROM t_lokalita WHERE nazov = 'Dubravy') ),
	(DEFAULT, 'FVE Dubravy 2', 0.850, 901.641, (SELECT id FROM t_lokalita WHERE nazov = 'Dubravy') ),
	(DEFAULT, 'FVE Plesivec', 0.984, 1150, (SELECT id FROM t_lokalita WHERE nazov = 'Plesivec') );

CREATE TABLE t_produkcia_den (
	id	serial	primary key,
	datum 	date NOT NULL,
	vykon	real,
	fve	integer	references t_fve(id)
);

CREATE TABLE t_produkcia_hodina (
	id	serial	primary key,
	cas 	timestamp NOT NULL,
	vykon	real,
	produkcia_den	integer	references t_produkcia_den(id)
);

select * from t_produkcia_hodina

CREATE TABLE t_predpoved_den (
	id	serial	primary key,
	lokalita	integer	references t_lokalita(id),
	datum 	date NOT NULL,
	sklon_den	smallint
);

CREATE TABLE t_predpoved_hodina (
	id	serial	primary key,
	predpoved_den	integer	references t_predpoved_den(id),
	cas 	timestamp NOT NULL,
	sklon_hodina	real,
	gho	smallint,
	oblacnost	smallint,
	teplota	real,
	rychlost_vetra	smallint,
	vlhkost	smallint,
	tlak	smallint
);

CREATE TABLE t_predpoved_import (
	datum	varchar(40),
	cas 	varchar(40),
	teplota	real,
	rychlost_vetra	smallint,
	smer_vetra smallint,
	oblacnost	smallint,
	vlhkost	smallint,
	tlak	smallint,
	gho	smallint,
	nic	varchar(1)
);


select * from t_predpoved_import

DELETE FROM t_predpoved_import

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