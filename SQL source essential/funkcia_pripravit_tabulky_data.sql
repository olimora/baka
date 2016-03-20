CREATE OR REPLACE FUNCTION pripravit_tabulky_data()
RETURNS integer AS
$$
BEGIN
-------------------------------------------------------------------------------------------------------
DROP TABLE IF EXISTS t_produkcia_import;
DROP TABLE IF EXISTS t_predpoved_import;
DROP TABLE IF EXISTS t_produkcia_hodina;
DROP TABLE IF EXISTS t_produkcia_den;
DROP TABLE IF EXISTS t_predpoved_hodina;
DROP TABLE IF EXISTS t_predpoved_den;
-------------------------------------------------------------------------------------------------------
CREATE TABLE t_predpoved_den (
	id		serial		PRIMARY KEY,
	lokalita	integer		REFERENCES t_lokalita,
	datum 		date 		NOT NULL
);
CREATE TABLE t_predpoved_hodina (
	id			serial		PRIMARY KEY,
	predpoved_den	integer		REFERENCES t_predpoved_den,
	cas 		timestamp 	NOT NULL,
	gho			smallint,
	oblacnost	smallint,
	teplota		real,
	vietor		smallint,
	vlhkost		smallint,
	tlak		smallint
);
CREATE TABLE t_produkcia_den (
	id		serial		PRIMARY KEY,
	fve		integer		REFERENCES t_fve,
	predpoved	integer		REFERENCES t_predpoved_den,
	datum 		date 		NOT NULL
);
CREATE TABLE t_produkcia_hodina (
	id		serial		PRIMARY KEY,
    fve		integer		REFERENCES t_fve,
	produkcia_den	integer		REFERENCES t_produkcia_den,
	predpoved	integer		REFERENCES t_predpoved_hodina,
	cas 		timestamp 	NOT NULL,
	vykon		real,
    praca		integer
);
CREATE TABLE t_produkcia_import (
	id		serial,
	in_cas		varchar(20),
	in_vykon	varchar(20),
	in_fve		varchar(20),
	cas_cet		timestamp with time zone,
	cas		timestamp,
	vykon		real,
    praca		integer,
	group_by_cas	timestamp
);
CREATE TABLE t_predpoved_import (
	datum		varchar(20),
	cas 		varchar(20),
	teplota		real,
	vietor		smallint,
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