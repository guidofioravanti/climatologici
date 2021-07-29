#' checkSeriesValidity
#'
#' Verifica la validità di una serie annuale secondo i controlli applicati
#' per la selezione delle serie SCIA utilizzate per la stima dei trend.
#'
#' \itemize{
#' \item L'86% dei dati deve essere valido
#' \item Un blocco di NA può al massimo essere lungo 4 anni
#' \item Gli ultimi 3 anni devono essere validi, non NA
#' }
#'
#' @param x Un oggetto ClimateData con serie annuali
#' @param percentuale.anni.validi Percentuale di anni validi nella serie (numeric)
#' @param max.size.block.na Massimo numero di NA consecutivi ammesso (integer)
#' @param ultimi_anni_validi Numero di anni che devono essere validi (non NA) a fine serie (integer)
#' @param lunghezza.minima.serie Lunghezza minima della seria, includendo anche i dati NA (integer)
#' @return Un oggetto ClimateData con le sole serie valide o NULL
#' @export

checkSeriesValidity<-function(x,percentuale.anni.validi,max.size.block.na,ultimi_anni_validi=0,lunghezza.minima.serie=0){

  stopifnot(is.ClimateData(x) && "yearly" %in% class(x))
  stopifnot(is.numeric(percentuale.anni.validi) && is.numeric(max.size.block.na) && is.numeric(ultimi_anni_validi))

  stopifnot(is.numeric(lunghezza.minima.serie) && (lunghezza.minima.serie>=0))


  controllo<-function(serie){

    length(serie)->lenSerie

    if(!lenSerie) return(FALSE)

    #la serie deve avere almeno il "percentualeDati"% dei dati
    floor(lenSerie*(percentuale.anni.validi/100) )->lunghezzaMinimaSerie
    which(!is.na(serie))->index.not.na
    if(length(index.not.na)<lunghezzaMinimaSerie) return(FALSE)

    #i blocchi di dati mancanti possono essere al più lunghi 4 anni
    verifica_blocchi_NA(serie,max.size.block.na=max.size.block.na)->risBlocchiNA
    if(risBlocchiNA$ris) return(FALSE)

    #le serie devono avere gli ultimi tre anni non tutti NA
    if(ultimi_anni_validi>0) if(any(is.na(serie[(lenSerie-(ultimi_anni_validi-1)):lenSerie]))) return(FALSE)

    return(TRUE)

  }

  if(nrow(x)<lunghezza.minima.serie) return(NULL)


  #risControllo: vettore di FALSE (serie non valida) o TRUE (serie valida) con cui filtriamo l'oggetto xts
  as.data.frame(zoo::coredata(x),optional=TRUE) %>%
    purrr::map_lgl(controllo)->risControllo

  if(all(risControllo==FALSE)) return(NULL)

  #restituisco x filtrato con le sole serie valide
  x[,names(x)[risControllo]]

}#fine checkSeries


#' checkSeriesValidity_default_values
#'
#' @param ... Non implementato
#'
#' @export

checkSeriesValidity_default_values<-purrr::partial(checkSeriesValidity,percentuale.anni.validi=86,max.size.block.na=4,ultimi_anni_validi=3,lunghezza.minima.serie=30)


#' checkSeriesValidity2
#'
#' NON IMPLEMENTATA, probabilmente inutile
#' Verifica la validità di una serie annuale secondo i controlli applicati
#' per la selezione delle serie SCIA utilizzate per la stima dei trend.
#'
#' \itemize{
#' \item L'86% dei dati deve essere valido
#' \item Un blocco di NA può al massimo essere lungo 4 anni
#' \item Gli ultimi 3 anni devono essere validi, non NA
#' }
#'
#' @param x Un oggetto ClimateData con serie annuali
#' @param percentuale.anni.validi Percentuale di anni validi nella serie (numeric)
#' @param max.size.block.na Massimo numero di NA consecutivi ammesso (integer)
#' @param lunghezza.minima.serie Numero di anni che devono essere validi (non NA) (integer)
#' @return Un oggetto ClimateData con le sole serie valide o NULL
#'
#'
checkSeriesValidity2<-function(x,percentuale.anni.validi,max.size.block.na,lunghezza.minima.serie=10)
{

  if(1==0){

  stopifnot(is.ClimateData(x) && "yearly" %in% class(x))
  stopifnot(is.numeric(lunghezza.minima.serie) || lunghezza.minima.serie>=0)
  stopifnot(is.numeric(percentuale.anni.validi) && is.numeric(max.size.block.na))

  controllo<-function(serie){

    imin<-1
    imax<-length(serie)

    ii<-1
    forzaIf<-FALSE

    while(TRUE){

      #questo if serve a gestire i casi in cui i controlli di qualita
      #falliscono e imax e imin puntano a valori non NA. Se non ci fosse
      #questo codice il programma girerebbe senza fermarsi. ii>1 perchè
      #nel primo ciclo questo controllo nn va fatto
      if(ii>1 && forzaIf){

        forzaIf<-FALSE
        which(is.na(serie))->index.na
        stopifnot(length(index.na)!=0)
        #degli NA ci debbono essere per forza altrimenti i controlli precedenti
        #avrebbero restituito serie di NA (serie non abbstanza lunga,minore di lunghezza.minima.serie)
        #oppure la serie vera e propria (serie valida)

        #imin potrebbe essersi spostato. lo stesso imax.
        #devo escludere gli NA sui cui già si sono mossi imin e/o imax
        #altrimenti loop infinito
        index.na[index.na>imin & index.na<imax]->index.na
        #anche in questo caso, index.na deve essere non nulla altrimenti i controlli
        #avrebbero fatto terminare la funzione

        min(index.na)->minNA
        max(index.na)->maxNA

        #a partire da imin cerco un NA, lo stesso a partire da imax
        #vedo la distanza tra l'NA e imin e imax. Mi sposto sull'Na
        #che dista meno da imin o imax. Voglio infatti mantenere la
        #parte della serie che ha più dati non NA
        if((minNA-imin)<=(imax-maxNA)){
          imin<-(minNA)
        }else{
          imax<-(maxNA)
        }

      } #su ii>1

      (imax-imin+1)->lenSubSerie

      #Se imin/imax (che vengono aggiornati ciclicamente) definiscono una serie di partenza inferiore a lunghezza.minima.serie inutile andare avanti.
      #Questo controllo non esclude che sia una serie con molti NA per cui il numero di dati non NA non raggiunge lunghezza.minima.serie.

      if(lenSubSerie<lunghezza.minima.serie){
        serie[1:length(serie)]<-NA
        return(serie)
      }

      #imin e imax puntano a due valori NA, restringo da entrambi i lati la serie
      if(is.na(serie[imin]) && is.na(serie[imax])){

        imin<imin+1
        imax<-imax-1
        next

      }else if(is.na(serie[imin])){

        imin<-imin+1
        next

      }else if(is.na(serie[imax])){

        imax<-imax-1
        next

      }#fine if

      #subSerie: gli estremi ora sono due non NA
      serie[imin:imax]->subSerie



      #ok ora subSerie identifica una porzione di serie tra due valori non NA e lunga almeno lunghezza.minima.serie.

      #Due domande:
      #1) I valori non NA in subSerie sono almeno pari a lunghezza.minima.serie? Questo serve a selezionare serie con almeno TOT anni validi
      #2) Ok ho almeno lunghezza.minima.serie dati validi. La serie comunque potrebbe essere formata da tantissimi NA
      #per cui la percentuale di dati validi non rispetti il vincolo dato da "percentualeDati".
      #Se ci fossero troppi NA devo continuare a cercare la sottoserie che contiene almeno lunghezza.minima.serie anni validi
      #ma senza avere al suo interno troppi NA
      floor(lenSubSerie*(percentuale.anni.validi/100) )->lunghezzaMinimaSerie
      length(subSerie[!is.na(subSerie)])->lenSenzaNA

      #se togliendo gli NA la serie è minore di lunghezzaMinimaSerie inutile andare avanti, a ogni ciclo la serie si restringe, quindi a un giro
      #successivo nn soddisferei questa condizione
      if((lenSenzaNA<lunghezzaMinimaSerie) || lenSenzaNA<lunghezza.minima.serie){serie[1:length(serie)]<-NA;return(serie)}

      #i blocchi di dati mancanti possono essere al più lunghi 4 anni
      verifica_blocchi_NA(subSerie,max.size.block.na=max.size.block.na)->risBlocchiNA
      #anche questo controllo fallito..ritorno su.
      #Devo incrementare ii perchèse  ora imin e imax puntano a due NA, da li non si schiodano
      #ma siccome i controlli sono falliti la funzione non termina. Devo forzare i min e imax
      #a muoversi verso un nuono NA. L'idea è eliminare parte dei dati validi
      #per trovare una sottoserie su cui rifare i controlli

      if(risBlocchiNA$ris){ii<-ii+1;forzaIf<-TRUE; next}

      #searrivo qui tutti i controlli sono passati!!! serie buona
      break

    }#fine su while

    # ifelse(imin-1>0,imin-1,1)->imin
    # ifelse(imax+1>length(serie),imax,imax+1)->imax

    #la parte fuori da imin:imax è la parte che nn permette di soddisfare i criteri di qualita
    #la devo annullare
    serie[-c(imin:imax)]<-NA
    return(serie)

  }


  #primo semplice controllo
  if(nrow(x)<lunghezza.minima.serie) return(NULL)



  #risControllo: vettore di FALSE (serie non valida) o TRUE (serie valida) con cui filtriamo l'oggetto xts
  as.data.frame(zoo::coredata(x),optional=TRUE) %>%
    purrr::map_dfc(controllo)->risControllo
    #purrrlyr::dmap(controllo)->risControllo

  risControllo %>% purrr::map_lgl(.f=function(x){any(!is.na(x))})->vvv
  if(all(vvv==FALSE)) return(NULL)

  #restituisco x filtrato con le sole serie valide
  x[,names(x)[vvv] ]

  }#if 1==0

}#fine checkSeries

