#' @title Busqueda aleatoria de funciones de uno o todos los paquetes
#'     instalados.
#'
#' @description Esta funcion permite obtener una funcion aleatoria de alguno de los
#'     paquete instalados  con fines de aprendizaje. Es decir,
#'     la idea es seleccionar al azar una funcion y la funcion que salga sorteada
#'     es con la que se debe experimentar para aprender sobre ella.
#'
#' @param package por defecto en la funcion esta cargado el paquete \code{base}.
#'      Pero se puede seleccionar cualquiera de los paquetes que fueron instalados
#'      en su computadora.
#'
#' @param ALL por defecto es \code{FALSE}. Si es \code{TRUE} significa que la busqueda
#'     aleatoria se va a buscar entre todos los paquetes que estan cargados en la
#'     sesion de trabajo actual.
#'
#' @return Esta funcion va a imprimir en pantalla la funcion sorteada, el pquete al
#'     que pertenece y la antidad de funciones que ese paquqte tiene. Adicionalmente
#'     se va a abrir un device de la ayuda de la funcion sorteada.
#'
#' @author Nicolas Schmidt \email{nschmidt@cienciassociales.edu.uy}
#'
#' @source http://bit.ly/Instructivo_package_IntRo
#'
#'
#' @examples
#'
#' rfunction()
#' rfunction(ALL = TRUE)
#' rfunction(package = "stats")
#'
#'
#' @export


rfunction <- function(package = "base", ALL = FALSE){

        r <- search()
        if(ALL){
                df <- stack(sapply(r[-1], ls))
                f  <- sample(df[[1]], 1)
                package <- stringr::str_extract(find(f),"(?<=:).*")
                l <- 1:nrow(df[df$ind == find(f),])
        }else{
                p <- paste0("package:", package)
                d <- FALSE
                if(!p %in% search()){
                        suppressMessages(require(package, character.only = TRUE))
                        d <- TRUE
                }
                l <- ls(search()[search() == p])
                f <- sample(l, 1)
                if(d){
                        int <- match(r, search())
                        end <- replicate(length(2:(int[2]-1)), detach(pos = 2))
                }
        }
        cat('\n - The function of the day is:', crayon::green$bold(f),
            '\n - Package:', crayon::blue(package),'-->', length(l), 'functions')
        help(f, package = noquote(package))

}
