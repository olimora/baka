--prvy experiment

--vybrane dni: vsetky, pre ktore idem predpovedat

select datum, sum(gho) gho, avg(teplota) teplota,
		sum(vietor) vietor, sum(praca) praca
from v_data
where fve_nazov = 'FVE Dubravy 2'
group by datum
order by datum

--poslem asi do funkcie //alebo mozem aj v Rku spracovat
vybrat 30 najpodobnejsich dni, pricom
celkova podobnost = 90*relativna podobnost v gho
					10* -//- v teplote
           	 		1* -//- vo vetre
relativna podobnost = percenta na kolko je hodnota s originalnou
		ak original gho 500, a toho zaznamu 400, tak
        relat je /*500......100%
        		   400......  x%*/
        		x=400*100/500
                100 nepotrebujeme, takze
                x = 400/500
