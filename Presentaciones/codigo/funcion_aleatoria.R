#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Curso IntRo
# Funcion de busqueda aleatoria de funciones
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

funcion_diaria <- function(paquete = "base"){
        r <- search()
        l <- ls(r[r == paste0("package:", paquete)])
        f <- sample(l, 1)
        cat("The function of the day is:", f)
        help(f)
}

funcion_diaria()
funcion_diaria("stats")


#------------------------------------------------------------
# GENERALIZANDO LA FUNCION A TODOS LOS PAQUETES DESCARGADOS
#------------------------------------------------------------
require("crayon")

funcion_diaria <- function(package = "base"){

        p <- paste0("package:", package)
        r <- search()
        d <- FALSE
        if(!p %in% search()){
                suppressMessages(require(package, character.only = TRUE))
                d <- TRUE
        }
        l <- ls(search()[search() == p])
        f <- sample(l, 1)
        cat('\n - The function of the day is:', green$bold(f),
            '\n - Package:', blue(package),'-->', length(l), 'functions')
        if(d){
                int <- match(r, search())
                end <- replicate(length(2:(int[2]-1)), detach(pos = 2))
        }
        help(f, package = noquote(package))

}

funcion_diaria(package = "pscl")
funcion_diaria(package = "tidyverse")
funcion_diaria("stats")



#------------------------------------------------------------
## sacar una funcion de todos los paquete cargados.
## agregando argumentos.
#------------------------------------------------------------
require("crayon")
require("stringr")

funcion_diaria <- function(package = "base", ALL = FALSE){

        r <- search()
        if(ALL){
                df <- stack(sapply(r[-1], ls))
                f  <- sample(df[[1]], 1)
                package <- stringr::str_extract(find(f),"(?<=:).*")
                l <- 1:nrow(df[df$ind==find(f),])
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
        cat('\n - The function of the day is:', green$bold(f),
            '\n - Package:', blue(package),'-->', length(l), 'functions')
        help(f, package = noquote(package))

}

funcion_diaria(ALL = TRUE)
funcion_diaria(package = "tidyverse")
funcion_diaria("stats")


