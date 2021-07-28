#' verifica_blocchi_NA:
#'
#' Funzione per identificare blocchi di NA contigui.
#'
#' verifica_blocchi_NA restituisce una lista di due elementi:
#' \itemize{
#' \item "ris": boolean pari a TRUE se il vettore x contiene almeno un blocco di NA di lunghezza superiore a max.size.block.na.
#' \item "nb": number of blocks, riporta il numero di blocchi in x che superano max.size.block.na.
#' }
#'
#' @param x Un vettore numerico.
#' @param max.size.block.na Lunghezza massima accettabile di NA contigui in x (integer).
#' @return list
#'
#' @export
#'
verifica_blocchi_NA<-function(x,max.size.block.na=3){

      if(missing(x)|| is.null(x) || !is.numeric(x)) stop("x deve essere un vettore numerico.")
      if(!length(x)) stop("Il vettore x deve avere lunghezza maggiore di zero.")

      which(is.na(x))->index.na

      #per identificare blocchi di NA contigui prima di tutto sostituiamo NA con un valore impossibile: -3333.
      #Questa trasformazione e' necessaria in quanto rle riporta ogni NA come un blocco di dimensione 1, anche quando contigui.
      if(!length(index.na)) return(list("ris"=FALSE,"nb"=0))

      x[index.na]<- -3333
      rle(x)->ris.rle
      ris.rle$lengths[ris.rle$values==-3333]->repeatedNA

      #ho trovato un blocco di NA più lungo di quanto permesso
      if(any(repeatedNA>max.size.block.na)) return(list("ris"=TRUE,"nb"=length(repeatedNA[repeatedNA>max.size.block.na])))

      return(list("ris"=FALSE,"nb"=length(repeatedNA[repeatedNA>max.size.block.na])))

}#fine verificaBlocchiNa



#' aggregaCD
#'
#' Funzioni per aggregare le serie di dati climatici
#'
#' @param x Un oggetto ClimateData.
#' @param max.na Massimo numero di NA che sono disposto a tollerare (integer)
#' @param rle.check Se TRUE viene verificato che all'interno di x non vi siano blocchi di NA di lunghezza superiore a "max.size.block.na"?
#' @param max.size.block.na Massimo numero di NA contigui (integer). Da usare con rle.check=TRUE.
#' @param ... Uno o piu' parametri in base alla tipologia di x.
#'
#' @return Un oggetto ClimateData.
#' @export
aggregaCD<-function(x,max.na,rle.check,max.size.block.na,...)
{
  stopifnot(is.ClimateData(x))
  UseMethod("aggregaCD",x)

}

#' @describeIn aggregaCD Funzione di default
#' @export
aggregaCD.default<-function(x,max.na,rle.check,max.size.block.na,...) { stop(glue::glue("Non so aggregare un oggetto di classe {class(x)}"))}

#' aggrega
#'
#' La funzione aggrega viene utilizzata sia da apply.monthly che da apply.yearly per
#' aggregare (sommare o mediare, in base al parametro) i dati di precipitazione/temperatura.
#' Nel caso dell'aggregazione annuale è necessario passare seasonal=TRUE in modo che venga verificata la validità di tutte le
#' stagioni in un anno. Una stagione si considera valida quando sono validi almeno due mesi su tre.
#'
#' @param x Un oggetto ClimateData.
#' @param aggFun Funzione per aggregare i dati in x: media o somma.
#' @param max.na Massimo numero di NA ammessi in x (integer).
#' @param rle.check Verificare o meno la presenza di blocchi di NA contigui? (Logical).
#' @param max.size.block.na Massimo numero di NA contigui (Integer). Questo parametro deve essere utilizzato con rle.check=TRUE.
#' @param seasonalCheck Verificare che tutte le stagioni siano valide? (Logical).
#'
#' @return double
#'
aggrega<-function(x,aggFun,max.na,rle.check,max.size.block.na,seasonalCheck){

  stagioni<-list("primavera"=c(3,4,5),"estate"=c(6,7,8),"autunno"=c(9,10,11),"inverno"=c(1,2,12))

  #aggrega lavora su oggetti xts. Ad esempio, per passare dal mensile all'annuale devo
  #fare un controllo sulle stagioni quindi ho bisogno dell'informazione temporale
  stopifnot(xts::is.xts(x))

  #controllo sui parametri
  stopifnot(is.numeric(max.na) || is.logical(rle.check) || is.numeric(max.size.block.na) || is.logical(seasonalCheck))

  which(is.na(x))->index.na
  length(index.na)->numeroNA

  #fallito già il primo controllo restituisco NA
  if(numeroNA > max.na) return(NA)

  #se voglio fare il controllo sulla presenza di blocchi di NA contigui
  if(rle.check && numeroNA){
    risBlocchiNA<-verifica_blocchi_NA(c(zoo::coredata(x)),max.size.block.na=max.size.block.na)$ris

    #se risBlocchiNA e' un intero maggiore di zero significa che almeno un blocco supera max.size.block.na.
    if(risBlocchiNA) return(NA)

  }

  #Per aggregare i mensili a livello annuale dobbiamo essere sicuri che tutte le stagioni siano valide
  #Una stagione è valida quando almeno due mesi su tre non sono NA
  if(seasonalCheck){

    #numeroNA >=3: questo controllo ha senso se si tratta di dati mensili. Se ho tre o piu' dati mensili NA allora faccio il controllo sulle stagioni
    if(numeroNA>=3){

      xts::.indexmon(x[index.na])+1->mesi

      #verifichiamo che ci siano almeno due stagioni valide
      unlist(lapply(stagioni,FUN=function(ss){length(mesi[mesi %in% ss])}))->len.out

      #len.out mi dice, per ogni stagione, quanti NA ho trovato. Se trovo un valore maggiore di 1
      #significa che almeno un stagione ha almeno due NA, peer cui l'anno non e' valido.
      if(any(len.out>1)) return(NA)

    }

  }#seasonalCheck

  aggFun(x,na.rm=TRUE)

}#aggrega



#' @describeIn aggregaCD Aggrega un ClimateData con serie giornaliere
#'
#' @export
aggregaCD.daily<-function(x,max.na,rle.check,max.size.block.na,...)
{

  #determina se si tratta di temperatura o precipitazione
  xts::xtsAttributes(x)$parametro->parametro

  #i giornalieri si aggregano diversamente a seconda del parametro in esame (temperatura o precipitazione)
  ifelse(parametro=="pr",sum,mean)->funzione

  #In base al tipo di dati di x determino il nuovo tipo di dati che verrà prodotto da aggregaCD.
  #Se si tratta di dati daily otterrò dati monthly.
  #Nel caso di dati monthly otterrò dati yearly.
  #In base al tipo di dati di output determino anche la funzione da applicare sulle serie ovvero:
  #apply.monthly (per i dati daily) o apply.yearly (per i dati monthly)

  purrr::map(1:ncol(x),.f=~(xts::apply.monthly(x[,.],
                                               FUN=aggrega,
                                               aggFun=funzione,
                                               max.na=max.na,
                                               rle.check=rle.check,
                                               max.size.block.na=max.size.block.na,
                                               seasonal=FALSE)

  ))->xx


  purrr::reduce(.x=xx,.f = xts::cbind.xts)->out
  ClimateData(x=out,param=parametro)->xx2

  #cbind aggiunge "X" davanti ai codici numerici
  names(xx2)<-stringr::str_replace_all(names(xx2),"^X","")

  xx2


}#fine aggregaCD.daily



#' @describeIn aggregaCD Aggrega un ClimateData con serie mensili
#'
#' @param ignore.par Se TRUE viene ignorato il tipo di parametro (precipitazione o temperatura) in fase di aggregazione.
#' @param seasonal Se TRUE viene controllata la validita' delle stagioni. Una stagione e' valida se contiene al massimo un mese NA.
#'
#' @export
#'
aggregaCD.monthly<-function(x,max.na,rle.check,max.size.block.na,ignore.par,seasonal,...)
{

  if(missing(ignore.par) || missing(max.na) || missing(rle.check) || missing(max.size.block.na) || missing(seasonal)) stop("Uno o piu' parametri mancanti")

  stopifnot(is.logical(ignore.par) || is.numeric(max.na) || is.logical(rle.check) || is.numeric(max.size.block.na) || is.logical(seasonal)
            )
  xts::xtsAttributes(x)$parametro->parametro

  #Per passare dai valori mensili agli annuali medio (tas) o sommo (pr). Questo non vale quando calcolo l'anomalia media annuale
  #per la precipitazione: la media appunto, non sommo le anomalie dei mesi. In questo caso uso ignore.par=TRUE
  ifelse(ignore.par,mean,ifelse(parametro=="pr",sum,mean))->funzione

  purrr::map(1:ncol(x),.f=~(xts::apply.yearly(x[,.],
                                         FUN=aggrega,
                                         aggFun=funzione,
                                         max.na=max.na,
                                         rle.check=rle.check,
                                         max.size.block.na=max.size.block.na,
                                         seasonal=seasonal)
  )) %>% purrr::reduce(.f=xts::cbind.xts)->ris

  names(ris)<-names(x)
  ClimateData(x=ris,param=parametro)

}#fine aggregaCD.monthly


#' @describeIn aggregaCD Aggrega un ClimateData con serie annuali.
#'
#' aggregaCD.yearly prende dati annuali e restituisce ancora un oggetto con classe "yearly".
#' Questa funzione viene utilizzata per creare un valore singolo (somma o media) di valori annuali
#' Per la precipitazione sono possibili due operazioni:
#' \itemize{
#' \item somma (se calcolo l'aggregato annuale)
#' \item media: l'operazione media viene utilizzata per costruire il climatologico mensile (ad esempio il climatologico del mese di gennaio)
#' }
#'
#'
#' @param ignore.par Se TRUE viene ignorato il tipo di parametro (precipitazione o temperatura) in fase di aggregazione.
#' @param seasonal Se TRUE viene controllata la validita' delle stagioni. Una stagione e' valida se contiene al massimo un mese NA.
#'
#' @export
#'
aggregaCD.yearly<-function(x,max.na,rle.check,max.size.block.na,ignore.par,seasonal,...){

  if(missing(ignore.par) || missing(max.na) || missing(rle.check) || missing(max.size.block.na) || missing(seasonal)) stop("Uno o piu' parametri mancanti")

  stopifnot(is.logical(ignore.par) || is.numeric(max.na) || is.logical(rle.check) || is.numeric(max.size.block.na) || is.logical(seasonal))

  xts::xtsAttributes(x)$parametro->parametro

  #se ignore.par==TRUE, dobbiamo fare la media. Questo serve per calcolare il climatologico mensile della precipitazione.
  #In ogni altro caso sara' il parametro a determinare la funzione di aggregazione.

  if(ignore.par){
    mean->funzione
  }else{

    if(parametro=="pr"){
      sum->funzione
    }else{
      mean->funzione
    }
  }

  #in base al tipo di dati di x determino il nuovo tipo di dati che verrà prodotto da
  #aggregaCD. Se si tratta di dati daily otterrò dati monthly. Nel caso di dati monthly
  #otterrò dati yearly. In base al tipo di dati di output determino anche la funzione
  #da applicare sulle serie ovvero: apply.monthly (per i dati daily) o apply.yearly (per i dati monthly)
  purrr::map(1:ncol(x),.f=~(aggrega(x[,.],
                                    aggFun=funzione,
                                    max.na=max.na,
                                    rle.check=rle.check,
                                    max.size.block.na=max.size.block.na,
                                    seasonal=seasonal))) %>%  purrr::flatten_dbl()->ris

  names(ris)<-names(x)

  #l'anno di riferimento è l'anno a metà tra l'inizio e la fine del periodo coperto con mese "01" e giorno "01".
  yearS<-lubridate::year(zoo::index(x)[1])
  yearE<-lubridate::year(zoo::index(x)[nrow(x)])
  mese<-lubridate::month(zoo::index(x)[1])

  xts::xts(x=as.data.frame(t(ris),optional=TRUE),
           order.by =as.Date(paste(floor((yearS+yearE)/2),mese,"01",sep="-"),format="%Y-%m-%d"))->xris
  rm(ris)

  class(xris)<-c("ClimateData","yearly",class(xris))
  xts::xtsAttributes(xris)<-list("parametro"=parametro)

  #ClimateData(x=xris,param=parametro) <--- non posso utilizzare ClimateData.xts perchè ho un singolo valore e il calcolo della periodicita fallisce

  xris

}#fine aggregaCD

