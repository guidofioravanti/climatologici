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
verifica_blocchi_NA<-function(x,max.size.block.na){

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
