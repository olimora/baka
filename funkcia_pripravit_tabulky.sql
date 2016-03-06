CREATE OR REPLACE FUNCTION pripravit_tabulky()
RETURNS integer AS 
$$
DECLARE
	path_import_aladin text;
	content_import_aladin text;
BEGIN
-------------------------------------------------------------------------------------------------------
DROP TABLE IF EXISTS t_produkcia_import;
DROP TABLE IF EXISTS t_predpoved_import;
DROP TABLE IF EXISTS t_produkcia_hodina;
DROP TABLE IF EXISTS t_produkcia_den;
DROP TABLE IF EXISTS t_predpoved_hodina;
DROP TABLE IF EXISTS t_predpoved_den;
DROP TABLE IF EXISTS t_fve;
DROP TABLE IF EXISTS t_lokalita;
-------------------------------------------------------------------------------------------------------
CREATE TABLE t_lokalita (
	id	serial 		PRIMARY KEY,
	nazov	varchar(20) 	NOT NULL
);
INSERT INTO t_lokalita VALUES
	(DEFAULT, 'Dubravy'),
	(DEFAULT, 'Plesivec');
	
CREATE TABLE t_fve (
	id 			serial		PRIMARY KEY,
	lokalita		integer		REFERENCES t_lokalita ON DELETE SET NULL,
	nazov			varchar(20) 	NOT NULL,
	instalovany_vykon	real,
	rocny_predpoklad	real	
);
INSERT INTO t_fve (nazov, instalovany_vykon, rocny_predpoklad, lokalita) VALUES
	('FVE Dubravy 1', 0.850, 901.641, (SELECT id FROM t_lokalita WHERE nazov = 'Dubravy') ),
	('FVE Dubravy 2', 0.850, 901.641, (SELECT id FROM t_lokalita WHERE nazov = 'Dubravy') ),
	('FVE Plesivec', 0.984, 1150, (SELECT id FROM t_lokalita WHERE nazov = 'Plesivec') );

CREATE TABLE t_predpoved_den (
	id		serial		PRIMARY KEY,
	lokalita	integer		REFERENCES t_lokalita ON DELETE SET NULL,
	datum 		date 		NOT NULL,
	sklon		smallint
);

CREATE TABLE t_predpoved_hodina (
	id		serial		PRIMARY KEY,
	predpoved_den	integer		REFERENCES t_predpoved_den ON DELETE SET NULL,
	cas 		timestamp 	NOT NULL,
	sklon_hodina	real,
	gho		smallint,
	oblacnost	smallint,
	teplota		real,
	rychlost_vetra	smallint,
	vlhkost		smallint,
	tlak		smallint
);

CREATE TABLE t_produkcia_den (
	id		serial		PRIMARY KEY,
	fve		integer		REFERENCES t_fve ON DELETE SET NULL,
	predpoved	integer		REFERENCES t_predpoved_den ON DELETE SET NULL,
	datum 		date 		NOT NULL,
	vykon		real	
);

CREATE TABLE t_produkcia_hodina (
	id		serial		PRIMARY KEY,
	produkcia_den	integer		REFERENCES t_produkcia_den ON DELETE SET NULL,
	predpoved	integer		REFERENCES t_predpoved_hodina ON DELETE SET NULL,
	cas 		timestamp 	NOT NULL,
	vykon		real	
);

CREATE TABLE t_produkcia_import (
	id		serial,
	in_cas		varchar(20),
	in_vykon	varchar(20),
	in_fve		varchar(20),
	cas_cet		timestamp with time zone,
	cas		timestamp,
	vykon		real,
	group_by_cas	timestamp
);

CREATE TABLE t_predpoved_import (
	datum		varchar(20),
	cas 		varchar(20),
	teplota		real,
	rychlost_vetra	smallint,
	smer_vetra 	smallint,
	oblacnost	smallint,
	vlhkost		smallint,
	tlak		smallint,
	gho		smallint,
	nic		varchar(1)
);
-------------------------------------------------------------------------------------------------------
RETURN 1;
END;
$$ 
LANGUAGE plpgsql