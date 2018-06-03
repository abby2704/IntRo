
#' @title Escala de notas.
#'
#' @description Esta funcion contiene la escala de notas. Convierte los puntajes
#'     de los ejercicios a la escala de notas de 0 a 12.
#' @param puntaje Numero entre 0 y 100.
#'
#' @return Numero entero entre 0 y 12.
#'
#' @examples
#'
#' escala(82)
#' escala(88.5)
#'
#' @export

escala <- function(puntaje){

        if(!is.numeric(puntaje)){
                stop("El puntaje debe ser un numero")
        }
        if(puntaje < 0 | puntaje > 100){
                stop("El puntaje debe ser un numero entre 0 y 100")
        }
        nota <- puntaje
        if ( puntaje < 50) nota = 0L
        if ( puntaje >=50 & puntaje < 58  ) nota = 6L
        if ( puntaje >=58 & puntaje < 66  ) nota = 7L
        if ( puntaje >=66 & puntaje < 74  ) nota = 8L
        if ( puntaje >=74 & puntaje < 82  ) nota = 9L
        if ( puntaje >=82 & puntaje < 90  ) nota = 10L
        if ( puntaje >=90 & puntaje < 96  ) nota = 11L
        if ( puntaje >=96 & puntaje <= 100) nota = 12L
        nota
}



