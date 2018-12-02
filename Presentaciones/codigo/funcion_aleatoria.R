#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Curso IntRo
# Funcion de busqueda aleatoria de funciones
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

funcion_diaria <- function(paquete = "base"){
        r <- search()
        l <- ls(r[which(r == paste0("package:", paquete))])
        f <- sample(l, 1)
        cat("The function of the day is:", f)
        help(f)
}

funcion_diaria()
funcion_diaria("stats")
