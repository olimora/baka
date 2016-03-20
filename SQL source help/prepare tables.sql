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

CREATE TABLE t_produkcia_import (
	id	serial	primary key,
	in_cas	varchar(40),
	in_vykon	varchar(20),
	in_fve	varchar(40),
	cas 	timestamp,
	vykon	real
);

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


