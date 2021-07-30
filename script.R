rm(list=objects())
library("guido")
#library("climatologici")
library("tidyverse")


readr::read_delim("Tmax_1868_2020_abruzzo.csv",delim=";",col_types =YYMMDD_TYPE) %>%
  dplyr::filter(yy>=1961)->dati

ClimateData(dati,param = "tmax")->cdati
aggregaCD(cdati,max.na = 10,rle.check = TRUE,max.size.block.na = 5)->mdati
aggregaCD(mdati,max.na = 3,rle.check = TRUE,max.size.block.na = 0,seasonal = TRUE,ignore.par = FALSE)->ydati

yearS<-2000
yearE<-2020
na.per.climatologico<-6
numero.anni.per.climatologico<-(yearE-yearS)-na.per.climatologico+1

checkSeriesValidity(ydati[lubridate::year(ydati) %in% ((yearE-numero.anni.per.climatologico+1):yearE),],
                    percentuale.anni.validi = 100,
                    max.size.block.na = 0,
                    ultimi_anni_validi = numero.anni.per.climatologico,
                    lunghezza.minima.serie = numero.anni.per.climatologico)->serieValidePerClimatologico

if(is.environment(mdati)) stop()

mdati[,names(mdati) %in% names(serieValidePerClimatologico)]->subM
climatologiciMensili(subM,yearS=yearS,yearE=yearE,max.na = na.per.climatologico,rle.check = FALSE,max.size.block.na = 0)->xx

#calcolo climatologico annuale
climatologicoAnnuale(xx)->annuale
