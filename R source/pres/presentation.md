<style>
.notimportant {
  color: 	#909090
}

</style>

PREDIKCIA VÝROBY ELEKTRINY Z OBNOVITEĽNÝCH ZDROJOV SO ZOHĽADNENÍM EXTERNÝCH FAKTOROV
========================================================
author: Oliver Moravčík
date: 14.4.2016
autosize: true
font-family: 'Helvetica'

Prehľad
========================================================
- Cieľ
  - predpovedať produkciu fotovoltaických elektrární na deň dopredu
- Dáta 
  - meteorologické predpovede od SHMÚ, model Aladin 
  - záznamy z produkcie FVE (fotovoltaických elektrární)
  - slnečné koordináty / solárne dáta
  - PostgreSQL
- Predikčný model
  - R
  - random forest

Dáta
========================================================
- Aladin
  - model numerickej predpovede počasia
  - 24 hodín dopredu 
  - čas v UTC, hodinové kroky
  - bodové záznamy => konkrétny bod na časovej osi
  - teplota
  - rýchlosť vetra
  - celková oblačnosť (%)
  - relatívna vlhkosť (%)
  - globálne ožiarenie

========================================================
<br>      
- FVE
  - čas v CEST, 15-minutové kroky
  - <div class="notimportant">výkon [kW]</div>
  - energia (práca) [kWh]
  - intervalové záznamy => záznam z 12:00 = 11:45 - 12:00
  - niekoľko chybových záznamov/dní

========================================================
- Slnečné koordináty / solárne dáta:
  - azimut (od východu na západ)
  - elevácia (výška Slnka na oblohe)
  - dĺžka dňa (počet hodín, kedy je Slnko nad horizontom)


```r
library(insol)
library(RPostgreSQL)
all_hours <- seq(ISOdate(2014,7,1,00), 
                 ISOdate(2015,11,1,00),by='hour')
sun_pos <- sunpos(sunvector(JD(all_hours), 
                            lat, long, tmz))
sun_pos <- cbind(sun_pos, 
                 elev = 90 - sun_pos[,'zenith'])
sun_pos <- data.frame(sun_pos, 
                  time = as.character(all_hours))
```

Metriky presnosti
========================================================
- hodnoty nameranej vyprodukovanej energie sú sčítané za celý deň
- predpovedaná hodnota - $y_{i,p}$, skutočná hodnota - $y_{i,s}$ 
- odmocnina zo strednej kvadratickej chyby
$$RMSE=\sqrt{\frac{1}{N}\sum_{i=1}^{N}(y_{i,p} - y_{i,s})^n}$$
- priemerná absolútna chyba
$$MAE=\frac{1}{N}\sum_{i=1}^{N}|y_{i,p} - y_{i,s}|$$

========================================================
- normalizované (%)
$$RRMSE=RMSE*\frac{100}{\frac{1}{N}\sum_{i=1}^{N}y_{i,s}}$$
<br><br>

```r
library(sirad)
statistics <- modeval(predicted, actual, stat=c("RMSE","RRMSE","MAE","RMAE"))
```

Random forest
========================================================
- náhodný les regresných stromov
- každý strom z náhodnej podmnožiny prediktorov
- každý uzol je vytvorený ako najlepšie možné rozdelenie, podľa prediktorov náhodne vybraných pri danom uzle
- výber prediktorov do podmnožín má normálne rozdelenie
- výstup je priemer hlasov (regresia) 


```r
library(randomForest)
forest <- randomForest(data=train_set,
  formula=praca~gho+oblacnost+teplota+dlzkadna)
output <- predict(forest, test_set, type="response", norm.votes=TRUE)
```

Nastavenia modelu
========================================================
- po dňoch: 
  - počet stromov: 700
  - počet uzlov stromu: 2 z 5
  - veľkosť trénovacej množiny: 110

- po hodinách:
  - počet stromov: 500
  - počet uzlov stromu: 2 z 7
  - veľkosť trénovacej množiny: 30
- počet uzlov stromu = odmicnina z počtu prediktorov
  
Výber trénovacej množiny
========================================================
- najpodobnejšie dni/hodiny
- faktory podobnosti:
  - globálne žiarenie: 90
  - teplota: 10
  - vietor: 1
  

```r
diff <- sapply(1:length(diff), function(x) {
  ret <- abs(hour[['gho']] - potencial[[x,'gho']]) * 100 / scale[['gho']] * 90
  return(ret)
})
```


Výsledky
========================================================
left: 50%
![plot of chunk unnamed-chunk-5](presentation-figure/unnamed-chunk-5-1.png)
***
- červená = RRMSE
- modrá = RMAE

- predch. 30 dní
- žiarenie * 90   
`+` teplota * 10   
`+` vietor * 1

Leto vs. zima
========================================================
left: 50%
- leto = 21. marec až 23. september = 636 dní
- zima = 24. september až 20. marec = 513 dní
- 2.4 x menšia chyba v lete

***
![plot of chunk unnamed-chunk-6](presentation-figure/unnamed-chunk-6-1.png)

========================================================
- malé hodnoty žiarenia
- nulové alebo minimálne hodnoty vyprodukovanej energie - zanedbateľné
- slnečný svit => žiarenie > 120 W/m2
- prvá a posledná hodina dňa = východ/západ slnka


dáta   | počet hodín | % hodín | % sum(energia)
-------|-------------|---------|---------------
všetky |      14 117 |  100.00 |    100.00
> 120  |       9 421 |   66.74 |     93.34
p. a p.|       7 265 |   51.46 |     83.65

========================================================
predikcia po dňoch:
![plot of chunk unnamed-chunk-7](presentation-figure/unnamed-chunk-7-1.png)

***
po hodinách:
![plot of chunk unnamed-chunk-8](presentation-figure/unnamed-chunk-8-1.png)

Najpresnejšia predikcia
========================================================
left:
- po hodinách
- faktory podobnosti:
  - žiarenie: 190    
  - oblačnosť: 100      
  - teplota: 30      
  - vietor: 5.5
  - vlhkosť: 1.5
  - dĺžka dňa: 53
  - elevácia: 1270

***
![plot of chunk unnamed-chunk-9](presentation-figure/unnamed-chunk-9-1.png)

========================================================
left: 50%
Výroba - priemer na deň
![plot of chunk unnamed-chunk-10](presentation-figure/unnamed-chunk-10-1.png)

***
Chyba predikcie
![plot of chunk unnamed-chunk-11](presentation-figure/unnamed-chunk-11-1.png)



Zrýchlenie výpočtov v R
========================================================
- alokácia pamäti
- vektorizácia (namiesto cyklov)
- matica namiesto tabuľky
- paralelizmus



```r
library(snow)
cl <- makeCluster(4, type='SOCK')
clusterEvalQ(cl, myFun <- function(x) UseMethod("myFun"))
clusterEvalQ(cl, { library(plyr); library(randomForest) })
clusterExport(cl, list("var1", "var2"))
output <- parSapply(cl, vec, function(x) {return(vec + var1 + var2)})
stopCluster(cl)
```

========================================================

![plot of chunk unnamed-chunk-13](presentation-figure/unnamed-chunk-13-1.png)
***
- a = prvotný kód
- b = alokácia pamäti, vektorizované výpočty, matica namiesto tabuľky
- c = paralelizované

Intel(R) Core(TM) i5-2450M CPU @ 2.50GHz

2 jadrá - 4 thready

Pokračovanie
========================================================
- zlepšiť výber najpodobnejších záznamov do trénovacej množiny
- vyskúšať iné predikčné metódy (neurónová sieť, quantile random forest, support vector machine)
  - potreba procesorového času
  - malý potenciál pre zlepšenie
- postprocessing - štatistická úprava predpovedí
  - veľký potenciál pre zlepšenie
  - veľká náročnosť 
  - potreba človekohodín 

========================================================
![plot of chunk unnamed-chunk-14](presentation-figure/unnamed-chunk-14-1.png)

========================================================
![plot of chunk unnamed-chunk-15](presentation-figure/unnamed-chunk-15-1.png)

========================================================
![plot of chunk unnamed-chunk-16](presentation-figure/unnamed-chunk-16-1.png)
