CREATE OR REPLACE FUNCTION pripravit_tabulky_experimenty()
RETURNS integer AS
$$
BEGIN
------------------------------------
DROP TABLE IF EXISTS t_experiment;
------------------------------------
CREATE TABLE t_experiment (
	id				serial		PRIMARY KEY,
    in_gho			boolean,
    in_teplota 		boolean,
    in_vietor		boolean,
    in_oblacnost	boolean,
    in_vlhkost		boolean,
    in_tlak			boolean,
    in_azim			boolean,
    in_zen			boolean,
    in_elev			boolean,
    in_dlzkadna		boolean,
    den_hod			varchar(3),
    fve				varchar(50),
    tren_mnoz_velkost	smallint,
    tren_mnoz_select	text,
    tren_mnoz_opis		text,
    MBE		real,
    RMBE	real,
    MSE		real,
    RMSE	real,
    RRMSE	real,
    MAE		real,
    RMAE	real,
    MAXAE	real,
    MAP 	real,
    SD		real
);
------------------------------------
RETURN 1;
END;
$$
LANGUAGE plpgsql