select MIN(sklon) from (
(select abs(to_date('2014-12-22','YYYY-MM-DD') - to_date('2015-5-01','YYYY-MM-DD')) sklon) 
UNION ALL
(select abs(to_date('2015-12-22','YYYY-MM-DD') - to_date('2015-5-01','YYYY-MM-DD')) sklon) 
UNION ALL
(select abs(to_date('2016-12-21','YYYY-MM-DD') - to_date('2015-5-01','YYYY-MM-DD')) sklon)) s1




select 1 WHERE "%DUB%" like 'C:\\000\\skola\\baka\\data\\Aladin\\gho\\ALADIN-DUB_48.587_19.369-2014-07'