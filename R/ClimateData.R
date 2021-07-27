#' checkParam
#'
#' Funzione per verificare il parametro passato a ClimateData
#'
#' @param x Stringa di caratteri
#' @return Una stringa di caratteri
#'
checkParam<-function(x){

  stringhePioggia<-c("pr","prcp","prpc","prec")
  stringheTemperatura<-c("tasmax","tasmin","tmax","tmin","tmean")

  stringhePossibili<-c(stringhePioggia,stringheTemperatura)

  if(is.null(x)) stop("param non puo' essere NULL")
  if(!is.character(x)) stop("param deve essere una stringa di caratteri valida")

  tolower(x)->x
  if(!(x %in%  stringhePossibili)) stop(glue::glue("La stringa {x} non e' corretta"))

  if((x=="prpc") || (x=="prcp")){
    x<-"pr"
  }else if(x=="tasmax"){
    x<-"tmax"
  } else if(x=="tasmin"){
    x<-"tmin"
  }

  return(x)

}#fine checkParam



#' ClimateData
#'
#' Funzioni per creare oggetti Climate Data
#'
#' Un oggetto ClimateData e' un oggetto xts con un attributo parametro.
#'
#' Un oggetto ClimateData puo' essere costruito a partire da un data.frame che deve contenere obbligatoriamente la colonna che identifica gli anni: yy o year.
#' Altre colonne opzionali sono: mm (o month) se si tratta di serie di dati mensili; dd (o day) se si tratta di serie di dati giornalieri.
#'
#' @param x Un dataframe o un oggetto xts da passare a ClimateData.
#' @param param Una stringa di testo (ammesse minuscole e maiuscole) per specificare se si tratta di dati di temperatura o precipitazione.
#'              Per definire serie di dati di temperatura sono ammesse le seguenti stringhe: "tas","tasmax","tasmin", "tmax","tmin","tmean".
#'              Per definire serie di dati di precipitazione sono ammesse le seguenti stringhe: "pr","prpc","prcp", "prec".
#'
#' @return Un oggetto di tipo ClimateData.
#' @export
ClimateData<-function(x,param)  UseMethod("ClimateData",x)

#' @describeIn ClimateData Crea un oggetto ClimateData a partire da un data.frame.
#' @export
ClimateData.data.frame<-function(x,param=NULL)
{

  stopifnot(is.data.frame(x) && ("yy" %in% names(x) || "year" %in% names(x)) )

  #se dd e' presente deve essere presente anche mm
  if("dd" %in% names(x) || "day" %in% names(x) ) stopifnot("mm" %in% names(x) || "month" %in% names(x))

  #verifica param
  checkParam(param)->param

  #ricodifichiamo year, month e day se presenti come nomi colonne
  names(x)[names(x) %in% c("year")]<-"yy"
  names(x)[names(x) %in% c("month")]<-"mm"
  names(x)[names(x) %in% c("day")]<-"dd"

  names(x)->nomi
  lStr<-list("daily"=c("yy","mm","dd"),"monthly"=c("yy","mm"),"yearly"=c("yy"))

  if("dd" %in% nomi){
    "daily"->tipoDati
    yy<-x$yy
    mm<-x$mm
    dd<-x$dd
  }else if("mm" %in% nomi){
    "monthly"->tipoDati
    yy<-x$yy
    mm<-x$mm
    dd<-rep("01",length(mm))
  }else{
    "yearly"->tipoDati
    yy<-x$yy
    mm<-rep("01",length(yy))
    dd<-rep("01",length(yy))
  }

  lStr[[tipoDati]]->stringa
  x[,!names(x) %in% stringa,drop=FALSE]->soloDati

  #caso assurdo che passo solo le colonne dei tempi
  if(!ncol(soloDati)) return(NULL)

  #variabile da parrase a xts per ordinare i dati
  tempo<-as.Date(paste(yy,mm,dd,sep="-"),format="%Y-%m-%d")

  xts::xts(x=as.data.frame(do.call("cbind",soloDati),optional=FALSE),order.by=tempo)->xDati
  class(xDati)<-c("ClimateData",tipoDati,class(xDati))
  xts::xtsAttributes(xDati)<-list("parametro"=param)

  xDati

}#fine funzione ClimateData


#' @describeIn ClimateData Crea un oggetto ClimateData partendo da un oggetto xts.
#' @export
ClimateData.xts<-function(x,param=NULL)
{

  #verifica il parametro
  checkParam(param)->param


  tryCatch({
    xts::periodicity(x)$scale
  },error=function(e){

    #nel caso dei climatologici annuali, periodicity fallisce
    if(nrow(x)==1){
      "yearly"
    }else{
      ""
    }

  })->periodicita

  if(!periodicita %in% c("daily","monthly","yearly")) stop("Periodicita' non riconosciuta")

  class(x)<-c("ClimateData",periodicita,class(x))
  xts::xtsAttributes(x)<-list("parametro"=param)

  x

}#fine funzione ClimateDataset


#' is.ClimateData
#'
#' Restituisce TRUE se si tratta di un oggetto ClimateData, altrimenti FALSE.
#'
#' @param x Un oggetto R.
#' @return Logical
#' @export
#'
is.ClimateData<-function(x)
{
  ifelse(xts::is.xts(x) && ("ClimateData" %in% class(x)),TRUE,FALSE)

}#fine is.ClimateData





#' as.data.frame.ClimateData
#'
#' Trasforma un oggetto ClimateData in un data.frame.
#'
#' @param x Un oggetto ClimateData.
#' @param ... Uno o piu' parametri per la funzione as.data.frame.
#'
#' @return Un dataframe.
#' @describeIn as.data.frame Crea un dataframe da un oggetto ClimateData.
#' @export
#'
#' @importFrom rlang .data
#'
as.data.frame.ClimateData<-function(x,...)
{
  as.data.frame(zoo::coredata(x),optional=TRUE)->y
  xts::periodicity(x)$scale->periodicita

  data.frame(yymmdd=as.character(zoo::index(x))) %>%
    tidyr::separate(col=.data$yymmdd,into=c("yy","mm","dd"),sep="-")->calendario

  if(periodicita=="yearly"){

    y %>%
      dplyr::mutate(yy=calendario$yy) %>%
        dplyr::select(.data$yy,dplyr::everything())

  }else if(periodicita=="monthly"){

    y %>%
      dplyr::mutate(yy=calendario$yy,mm=calendario$mm) %>%
        dplyr::select(.data$yy,.data$mm,dplyr::everything())

  }else if(periodicita=="daily"){

    y %>%
      dplyr::mutate(yy=calendario$yy,mm=calendario$mm,dd=calendario$dd) %>%
        dplyr::select(.data$yy,.data$mm,.data$dd,dplyr::everything())

  }else{

    stop("Periodicita' non riconosciuta")

  }


}#fine as.data.frame


#' all_NA
#'
#' Verifica se un oggetto ClimateData sia composto di soli NA
#' Utile ad esempio, nel calcolo dei climatologici mensili.
#' @param x Un oggetto ClimateData
#' @return Logical
#' @export
#'
all_NA<-function(x){
  all(purrr::map(is.na(x),all) %>% purrr::flatten_lgl())
}
