#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Curso IntRo
# Funcion de busqueda aleatoria de funciones
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

funcion_diaria <- function(paquete = "base"){
        ruta <- search()
        l <- ls(ruta[which(ruta == paste0("package:", paquete))])
        sample(l, 1)
}

funcion_diaria()
funcion_diaria("stats")


