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