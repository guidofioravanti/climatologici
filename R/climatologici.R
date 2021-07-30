#' climatologiciMensili
#'
#'  Utilizza la funzione "calcolaClimatologico" per il calcolo dei valori climatologici mensili e annuali secondo i criteri del WMO (vedi seguito).
#'  Calcolo dei valori climatologici secondo i criteri del WMO.
#'  \itemize{
#'  \item Dai dati giornalieri (oggetto ClimateData daily) calcolare l'aggregato mensile con aggregateCD
#'  \item Utilizzare la funzione climatologiciMensili per ottenere il climatologico di ogni mese. I dati cosi' ottenuti sono oggetti ClimateData monthly.
#'  \item Utilizzare aggregaeCD (per l'esattezza: aggregaCD.monthly) con max.na=0 per ottenere il climatologico annuale.
#'  }
#'
#'
#'  Attenzione: l'output e' un tibble e non un ClimateData non per errore. Se fosse un ClimateData avrebbe periodicita' mensile
#'  che pero' creerebbe problemi ad aggregaCD per calcolare il climatologico annuale. Il nodo corretto di procedere e' quello di
#'  prendere il tibble di output, eliminare la colonna dei mesi e creare un oggetto ClimateData. Questo avra' periodicita' annuale (mancando la colonna dei mesi).
#'  L'oggetto ClimateData puo' essere poi passato ad aggregateCD per creare il climatologico annuale.
#'
#'
#' @param x Oggetto ClimateData con serie mensili
#' @param yearS Anno di inizio per il calcolo del climatologico (start year)
#' @param yearE Anno di fine per il calcolo del climatologico (end year)
#' @param max.na Massimo numero di NA ammessi nella serie.
#' @param rle.check Controllare la presenza di blocchi di NA? (logical)
#' @param max.size.block.na Numero massimo di NA consecutivi (da usare con rle.check=TRUE)
#' @return Oggetto ClimateData con valori climatologici mensili.
#'
#' @export
#'
climatologiciMensili<-function(x,yearS,yearE,max.na,rle.check,max.size.block.na)
{
  stopifnot(is.ClimateData(x) && ("monthly" %in% class(x)))

  if(missing(yearS) || missing(yearE)) stop("Specificare anno inizio/fine calcolo climatologico, parametri yearS/yearE")
  stopifnot(is.numeric(yearS) && is.numeric(yearE))


  if(missing(max.na) || missing(rle.check) || missing(max.size.block.na))
    stop("Specificare max.na, rle.check e max.size.block.na")

  stopifnot(is.numeric(max.na) && is.logical(rle.check) && is.numeric(max.size.block.na))

  numeroMinimoAnni<-((yearE-yearS+1)-max.na)
  stopifnot(numeroMinimoAnni>0)

  x[paste0(yearS,"/",yearE)]->subx

  #subDati è vuoto
  if(!nrow(subx)) stop("Nessun periodo selezionato, yearS e yearE sono validi?")

  floor((yearE+yearS)/2)->annoOut
  seq.Date(from=as.Date(paste0(annoOut,"-01-01")),to=as.Date(paste0(annoOut,"-12-01")),by="month")->indice


  #Altro controllo da fare: se i dati partono dal 1961 e voglio calcolare
  #il climatologico 1951/1980 ammettendo al massimo 6 NA devo prima di tutto
  #verificare che subx contenga almeno 24 anni distinti. Se non faccio questo controllo
  #ottengo, ad esempio, che subx ha i valori dal 1961 al 1980 tutti completi per cui
  #aggregaCD fa le mie aggregazioni ...ma su 20 anni di dati!! Quando io ne devo avere
  #almeno 24!! Quindi: verifichiamo che in subx ci siano 24 anni distinti altrimenti
  #restituisco NA
  unique(lubridate::year(zoo::index(subx)))->anniDistinti

  if(length(anniDistinti)<numeroMinimoAnni){
    ncol(x)->colonne
    matrix(rep(NA_integer_,colonne*12),nrow = 12,ncol=colonne)->matriceNA
    colnames(matriceNA)<-names(x)

    tibble::as_tibble(matriceNA) %>%
      dplyr::mutate(yy=annoOut,mm=1:12) %>%
      dplyr::select(.data$yy,.data$mm,dplyr::everything())->finale

    attr(finale,"parametro")<-xts::xtsAttributes(x)$parametro


    return(finale)

  }


  purrr::map(0:11,.f=function(mese){

    #ad esempio, prendo tutti i Gennaio tra il 1961 e il 1990

    subx[xts::.indexmon(subx)==mese,]->serieMeseAnni
    class(serieMeseAnni)[class(serieMeseAnni)=="monthly"]<-"yearly"

    #ignore.par serve per utilizzare l'operazione "media" anche se si tratta di precipitazione
    aggregaCD(x=serieMeseAnni,ignore.par=TRUE,max.na=max.na,rle.check=rle.check,max.size.block.na=max.size.block.na,seasonal=FALSE)

  }) %>% purrr::reduce(.f=dplyr::bind_rows)->ris

  names(ris)<-names(x)

  ris %>%
    dplyr::mutate(yy=annoOut,mm=1:12) %>%
    dplyr::select(.data$yy,.data$mm,dplyr::everything())->finale

  attr(finale,"parametro")<-xts::xtsAttributes(x)$parametro

  finale

}



#' climatologicoAnnuale
#'
#' Funzione per il calcolo del climatologico annuale
#'
#' @param x Un tibble con colonne yy e mm. Si tratta del tibble restituito da climatologiciMensili.
#' @return Un data.frame con i valori climatologici annuali.
#'
#' @export
#'
climatologicoAnnuale<-function(x){

  stopifnot(tibble::is.tibble(x))
  stopifnot("yy" %in% names(x))
  stopifnot("mm" %in% names(x))

  ClimateData(x %>% dplyr::select(-.data$mm),param=attr(x,"parametro"))->cmensili
  aggregaCD(cmensili,max.na=0,max.size.block.na = 0,rle.check =FALSE,ignore.par = FALSE,seasonal = FALSE)->annuale

  as.data.frame(zoo::coredata(annuale))->df

  df %>%
    dplyr::mutate(yy=lubridate::year(annuale)) %>%
    dplyr::select(.data$yy,dplyr::everything())


}
