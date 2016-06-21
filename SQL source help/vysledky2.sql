
select id, cas_behu, metoda, fve, param1, param2, param3, param4, param5, n, rmse, rrmse, mae, RMAE, SD, pod_elev, pod_gho, pod_oblacnost, pod_teplota, pod_vietor, pod_dlzkadna, pod_vlhkost, pod_elev
from t_experiment
where metoda = 'stats_hod'
and cas_behu >= to_timestamp('06.06.2016 11:00', 'DD.MM.YYYY HH24:MI')
order by cas_behu desc

select id, cas_behu, metoda, fve, param1, param2, param3, param4, param5, n, rmse, rrmse, mae, RMAE, SD, pod_elev, pod_gho, pod_oblacnost, pod_teplota, pod_vietor, pod_dlzkadna, pod_vlhkost, pod_elev
from t_experiment
where metoda = 'stats_den'
and cas_behu >= to_timestamp('06.06.2016 11:00', 'DD.MM.YYYY HH24:MI')
order by cas_behu desc


CREATE TABLE t_importance (
	id 			serial		PRIMARY KEY,
    pokus 		INTEGER,
    mse_gho		REAL,
    mse_obl		real,
    mse_tep		real,
    mse_vie		real,
    mse_vlh		real,
    mse_dlz		real,
    mse_ele		REAL,
    pur_gho		real,
    pur_obl		real,
    pur_tep		real,
    pur_vie		real,
    pur_vlh		real,
    pur_dlz		real,
    pur_ele		real
);

select pokus,
avg(mse_gho), min(mse_gho), max(mse_gho),
avg(mse_obl), min(mse_obl), max(mse_obl),
avg(mse_tep), min(mse_tep), max(mse_tep),
avg(mse_vie), min(mse_vie), max(mse_vie),
avg(mse_vlh), min(mse_vlh), max(mse_vlh),
avg(mse_dlz), min(mse_dlz), max(mse_dlz),
avg(mse_ele), min(mse_ele), max(mse_ele),
avg(pur_gho), min(pur_gho), max(pur_gho),
avg(pur_obl), min(pur_obl), max(pur_obl),
avg(pur_tep), min(pur_tep), max(pur_tep),
avg(pur_vie), min(pur_vie), max(pur_vie),
avg(pur_vlh), min(pur_vlh), max(pur_vlh),
avg(pur_dlz), min(pur_dlz), max(pur_dlz),
avg(pur_ele), min(pur_ele), max(pur_ele)
from t_importance
group by pokus
;