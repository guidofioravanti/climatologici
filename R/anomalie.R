#' checkClimatol
#'
#' @param climatol Climatologia
#' @return NULL
checkClimatol<-function(climatol){

  #verifichiamo l'indice di climatol
  stopifnot(is.ClimateData(climatol) && "yearly" %in% class(climatol))
  stopifnot(length(unique(lubridate::years(zoo::index(climatol))))==1) #un unico anno
  stopifnot(length(unique(months(zoo::index(climatol))))==12) #12 mesi tutti distinti

}#fine checkClimatol



#' OptWithClimatol
#'
#' Differenza tra ciascuna serie mensile e i valori climatologici mensili
#'
#' @param fun Funzione da applicare.
#'
#'
OpWithClimatol<-function(fun){

  function(x,y){

    purrr::map2(.x=as.data.frame(zoo::coredata(x),optional=TRUE),.y=as.data.frame(zoo::coredata(y),optional=TRUE),.f=function(x,y){

      as.numeric(fun(x,y))

    })

  }#fine diffWithClimatol

}#fine



diffWithClimatol<-OpWithClimatol(fun=`-`)
sumWithClimatol<-OpWithClimatol(fun=`+`)



#####################################
#ANOMALIE:
#
# Calcolo anomalie mensili. Parto dalle serie mensili dei dati e sottraggo la media
# climatologica. La media climatologica può essere calcolata mediante i criteri del WMO
# o seguendo dei criteri più blandi.


#' anomalie
#'
#' Funzioni per il calcolo delle anomalie mensili e annuali.
#'
#' @param x Oggetto ClimateData per serie mensili o annuali.
#' @param climatol Valori climatologici rispetto a cui calcolare l'anomalia.
#'
#' @return Oggetto ClimateData con le anomalie mensili o annuali di temperatura o precipitazione.
#'
#' @export
#'
anomalie<-function(x,climatol)
{
  UseMethod("anomalie",x)
}

#' @describeIn anomalie Calcolo anomalie mensili.
#'
#'
#' @export
#'
anomalie.monthly<-function(x,climatol)
{
  stopifnot(is.ClimateData(x) && "monthly" %in% class(x))
  stopifnot((xts::xtsAttributes(x)$parametro)==(xts::xtsAttributes(climatol)$parametro))
  stopifnot(names(climatol)==names(x))
  checkClimatol(climatol)

  xts::xtsAttributes(x)$parametro->parametro

  #differenza tra ciascuna serie mensile e i valori climatologici mensili
  diffWithClimatol(x,climatol)->ris

  xts::xts(as.data.frame(ris,optional=TRUE),order.by = zoo::index(x))->xris
  rm(ris)
  class(xris)<-c("ClimateData","monthly",class(xris))
  xts::xtsAttributes(xris)<-list("parametro"=xts::xtsAttributes(x)$parametro)

  xris

}#fine anomalie.monthly



#' @describeIn anomalie Calcolo delle anomalie annuali.
#'
#' @export
anomalie.yearly<-function(x,climatol)
{
  stopifnot(is.ClimateData(x) && "yearly" %in% class(x))
  stopifnot(is.ClimateData(climatol) && "yearly" %in% class(climatol))
  stopifnot((xts::xtsAttributes(x)$parametro)==(xts::xtsAttributes(climatol)$parametro))
  stopifnot(names(climatol)==names(x))

  xts::xtsAttributes(x)$parametro->parametro
  #differenza tra ciascuna serie annuale e i valori climatologici mensili
  diffWithClimatol(x,climatol)->ris

  xts::xts(as.data.frame(ris,optional=TRUE),order.by = zoo::index(x))->xris
  rm(ris)
  class(xris)<-c("ClimateData","yearly",class(xris))
  xts::xtsAttributes(xris)<-list("parametro"=xts::xtsAttributes(x)$parametro)

  xris

}#fine anomalie.yearly





#' ricostruisciSerie
#'
#' Operazione inversa a quella effettuata da anomalie
#' Prende un oggetto prodotto da anomalie e ricostruisce le serie.
#'
#' @param x Serie di anomalie mensili o annuali.
#' @param climatol Valori climatologici mensili o annuali.
#'
#' @return ClimateData con le serie mensili o annuali di temperatura o precipitazione.
#'
#' @export
ricostruisciSerie<-function(x,climatol)
{

  UseMethod("ricostruisciSerie",x)
}


#' @describeIn ricostruisciSerie Ricostruisce le serie mensili.
#'
#' @export
ricostruisciSerie.monthly<-function(x,climatol)
{

  stopifnot(is.ClimateData(x) && "monthly" %in% class(x))
  stopifnot(names(climatol)==names(x))
  stopifnot((xts::xtsAttributes(x)$parametro)==(xts::xtsAttributes(climatol)$parametro))
  checkClimatol(climatol)

  #differenza tra ciascuna serie mensile e i valori climatologici mensili
  sumWithClimatol(x,climatol)->ris

  xts::xts(as.data.frame(ris,optional=TRUE),order.by = zoo::index(x))->xris
  rm(ris)

  ClimateData(x=xris,param=xts::xtsAttributes(x)$parametro)

}#fine anomalie.monthly


#' @describeIn ricostruisciSerie Ricostruisce le serie annuali.
#'
#' @export
ricostruisciSerie.yearly<-function(x,climatol)
{

  stopifnot(is.ClimateData(x) && "yearly" %in% class(x))
  stopifnot(is.ClimateData(climatol) && "yearly" %in% class(x))
  stopifnot(names(climatol)==names(x))
  stopifnot((xts::xtsAttributes(x)$parametro)==(xts::xtsAttributes(climatol)$parametro))


  #differenza tra ciascuna serie mensile e i valori climatologici mensili
  sumWithClimatol(x,climatol)->ris

  xts::xts(as.data.frame(ris,optional=TRUE),order.by = zoo::index(x))->xris
  rm(ris)

  ClimateData(x=xris,param=xts::xtsAttributes(x)$parametro)

}#fine anomalie.yearly







