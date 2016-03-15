CREATE OR REPLACE FUNCTION pripravit_tabulky()
RETURNS integer AS 
$$
BEGIN
-------------------------------------------------------------------------------------------------------
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
-------------------------------------------------------------------------------------------------------
RETURN 1;
END;
$$ 
LANGUAGE plpgsql