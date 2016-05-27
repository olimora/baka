#precitanie csv

vstup <- read.csv(file="C:\\000\\skola\\baka\\data\\Aladin\\gho\\2014-07\\ALADIN-DUB_48.587_19.369-2014-07.csv",head=F,sep=",")

print(vstup)

?read.c

colnames(vstup) <- c("datum", "cas", "teplota", "rychlost_vetra", "smer_vetra", 
                     "oblacnost", "vlhkost", "tlak", "gho", "nic")

bez_smeru_nic <- vstup[,c(1:4,6:9)]

bez_null_gho <- subset(bez_smeru_nic, gho > 0)

cas_ok <- within(bez_null_gho, cas <- paste(datum, cas, sep=' '))

pripravene <- cas_ok[2:8]

pripravene[,c("cas", "gho", "oblacnost", "teplota", "rychlost_vetra", "vlhkost", "tlak")]

files<-list.files("C:\\000\\skola\\baka\\data\\Aladin\\gho")

lll <- "(),(),(),"

gsub(",$", ";", lll) 