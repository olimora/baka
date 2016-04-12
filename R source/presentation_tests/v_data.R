cc <- dbGetQuery(db.con, "select count(*) as cc from (select distinct * from (select cas, fve from v_data) s1) s2")$cc
perc <- cc * 100 / cc_all
summ <- dbGetQuery(db.con, "select sum(praca) cc from v_data;")$cc
perc_summ <- summ * 100 / sum_all


cc_120 <- dbGetQuery(db.con, "select count(*) as cc from (select distinct * from (select cas, fve from v_data_120) s1) s2")$cc
perc_120 <- cc_120 * 100 / cc_all
sum_120 <- dbGetQuery(db.con, "select sum(praca) cc from v_data_120;")$cc
perc_sum_120 <- sum_120 * 100 / sum_all


cc_all <- dbGetQuery(db.con, "select count(*) as cc from (select distinct * from (select cas, fve from v_data_all) s1) s2")$cc
sum_all <- dbGetQuery(db.con, "select sum(praca) cc from v_data_all;")$cc

print(paste(cc, perc, summ, perc_summ))
print(paste(cc_120, perc_120, sum_120, perc_sum_120))
print(paste(cc_all, sum_all))