#' @title Autoevaluacion de 'Ejercicios II' (Modulo Matrices).
#'
#' @description Es una funcion que permite autoevaluar los ejercicios del modulo
#'     de matrices del curso Introduccion al software estadistico R.
#' @param nombre Una cadena de caracteres que indique el nombre del estudiante.
#' @param apellido Una cadena de caracteres que indique el apellido del estudiante.
#' @param mail Una cadena de caracteres que indique el mail del estudiante.
#' @param intentos Refiere a la cantidad de veces que se realizo la autoevaluacion
#'     hasta el momento en el que se resuelve que los ejercicios estan prontos para
#'     ser enviados para obtener la nota final. Por defeto es 1. La cantidad de
#'     intentos debe ser un numero entero. Si el numero no es entero la funcion
#'     va a forzar el numero a uno de tipo  \code{integer} redondeando hacia
#'     arriba.
#' @param enviar Valor logico que por defeto es \code{FALSE}. En el momento en el que el
#'     estudiante considera que los ejercicios estan completos debe enviar los
#'     resultados para obtener la nota final de los ejercicios. La nota final le va
#'     a llegar al mail que cargo en el argumento \code{mail} de esta funcion.
#' @param summary Valor logico que por defetco es \code{TRUE}. La utilidad de este
#'     argumento es contar con un resumen de la situacion de todos los ejercicios
#'     correspondientes al modulo de matrices.
#'
#' @examples
#'
#' mat1 <- as.matrix(1L)
#'
#' auto_eval_matrix(nombre = "Nicolas",
#'                  apellido = "Schmidt",
#'                  mail = NULL,
#'                  enviar = FALSE,
#'                  intentos = 3,
#'                  summary = TRUE)
#' @export


auto_eval_matrix <- function(nombre = NULL,
                             apellido = NULL,
                             mail = NULL,
                             intentos = 1,
                             enviar = FALSE,
                             summary = TRUE){

        espacio <- ls(search()[1])
        cant_ej <- 7
        if(length(espacio)==0){
                stop("No hay ningun objeto creado en la sesion de trabajo. Puede consultarlo con la funcion 'objects()' o 'ls()' ")
        }
        vector_objetos <- paste0("mat", 1:cant_ej)
        faltantes <- vector_objetos %in% espacio
        if(sum(faltantes)==0){
                stop("En el aspacio de trabajo no hay ninguno de los objeto del ejercicio creados. Recuerde que debe utilizar los mombres que figuran en los ejercicios")
        }

        base <- as.list(stats::setNames(rep(NA, cant_ej), paste("Ejercicio",1:cant_ej)))

        if(vector_objetos[1] %in% espacio){
                if(sum(dim(mat1))!=2){
                        warning("Ejercicio 1: La dimension de la matriz no es correcta.")
                }
                if(!is.integer(mat1)){
                        warning("Ejercicio 1: El tipo de dato no es correcto. Debe ser un numero entero.")
                }
                base[[1]] <- ifelse(sum(dim(mat1)) == 2 && is.integer(mat1), 1, 0)
        }
        if(vector_objetos[2] %in% espacio){
                if(sum(dim(mat2))!=7){
                        warning("Ejercicio 2: La dimension de la matriz no es correcta.")
                }
                if(!all(mat2 %in% 1:12)){
                        warning("Ejercicio 2: Los numeros que debe tener la matriz son del 1 al 12.")
                }
                if(is.null(dimnames(mat2))){
                        warning("Ejercicio 2: Debe ponerle nombre a las filas y a las columnas.")
                }
                if(ncol(mat2)!=4){
                        warning("Ejercicio 2: La matriz debe tener 3 filas y 4 columnas.")
                }
                base[[2]] <- ifelse(all(mat2 %in% 1:12) && !is.null(dimnames(mat2)) && ncol(mat2)==4, 1, 0)
        }
        if(vector_objetos[3] %in% espacio){
                if(sum(dim(mat3))!=4){
                        warning("Ejercicio 3: La dimension de la matriz no es correcta.")
                }
                if(sum(is.na(mat3))!=4){
                        warning("Ejercicio 3: La matriz solo debe contener valores NA.")
                }
                base[[3]] <- ifelse(sum(dim(mat3)) == 4 && sum(is.na(mat3)) == 4, 1, 0)
        }
        if(vector_objetos[4] %in% espacio){
                if(!all(c(2.5, 6.5, 10.5) %in% mat4)){
                        warning("Ejercicio 4: Los valores promedio por fila no son correctos, busque en la ayuda de la funcion addmargins o apply.")
                }
                if(length(colnames(mat4)) == 4){
                        warning("Ejercicio 4: Falta el nombre de la columna que contiene la media.")
                }
                base[[4]] <- ifelse(all(c(2.5, 6.5, 10.5) %in% mat4) && length(colnames(mat4)) == 5, 1, 0)
        }
        if(vector_objetos[5] %in% espacio){
                if(sum(dim(mat5))!=20){
                        warning("Ejercicio 5: La dimension de la matriz no es correcta.")
                }
                if(sum(mat5 == 1) != 45){
                        warning("Ejercicio 5: La cantidad de unos no es correcta.")
                }
                if(sum(mat5 == 0) != 45){
                        warning("Ejercicio 5: La cantidad de ceros no es correcta.")
                }
                if(!all(diag(mat5) == 10)){
                        warning("Ejercicio 5: La diagonal principal no contiene la cantidad adecuada de valores iguales a 10.")
                }
                base[[5]] <- ifelse(sum(mat5 == 1) == 45 && sum(mat5 == 0) == 45 && sum(mat5 == 10) == 10, 1, 0)
        }
        if(vector_objetos[6] %in% espacio){
                if(sum(dim(mat6))!=20){
                        warning("Ejercicio 6: La dimension de la matriz no es correcta.")
                }
                if(!is.character(mat6)){
                        warning("Ejercicio 6: Los datos de la matriz deben ser de tipo character.")
                }
                if(length(unique(mat6))!=100){
                        warning("Ejercicio 6: No se pueden repetir los datos, las 100 entradas de datos deben ser distintas.")
                }
                contenido <- any(grepl(paste(c(letters, LETTERS), collapse = "|"), mat6))
                if(!contenido){
                        warning("Ejercicio 6: Las entradas de datos deben contener letras o palabras. No pueden tener solo numeros.")
                }
                base[[6]] <- ifelse(contenido && length(unique(mat6))==100, 1, 0)
        }
        if(vector_objetos[7] %in% espacio){
                if(!is.matrix(mat7)){
                        warning("Ejercicio 7: El objeto 'mat7' debe ser una matriz.")
                }
                if(!all(colnames(mat7) == colnames(datasets::iris[,-5]))){
                        warning("Ejercicio 7: Los nombres de las variables no coiciden con los nombres de la base 'iris'.")
                }
                if(!all(apply(mat7, 2, typeof) == "double")){
                        warning("Ejercicio 7: Las variables deben ser de tipo 'numeric'.")
                }
                if(!all(c(paste("Observacion", 1:150, sep="_"), "Media") %in% rownames(mat7))){
                        warning("Ejercicio 7: Recuderde que el nombre de las filas debe ser 'Observacion_1', 'Observacion_2'... y la ultima fila debe tener el nombre de 'Media'. ")
                }
                if(!all(c(5.843, 3.057, 3.758, 1.199) %in% mat7)){
                        warning("Ejercicio 7: Los promedios por variable no son correctos. Recuerde que debe dejar tres decimales. ")
                }

                base[[7]] <- ifelse(all(apply(mat7, 2, typeof) == "double") &&
                                        all(colnames(mat7) == colnames(datasets::iris[,-5])) &&
                                            all(c(paste("Observacion", 1:150, sep="_"), "Media") %in% rownames(mat7)) &&
                                                all(c(5.843, 3.057, 3.758, 1.199) %in% mat7), 1, 0)
        }

        base1 <- do.call(rbind, base)
        base2<- cbind(base1, base1[,1], base1[,1])
        colnames(base2)<-c("Correcta", "Incorrecta", "Incompleta")
        base2[,1] <- ifelse(is.na(base2[,1]) | base2[,1]==0, 0, 1)
        base2[,2] <- ifelse(is.na(base2[,2]) | base2[,2]==1, 0, 1)
        base2[,3] <- ifelse(is.na(base2[,3]), 1, 0)
        base2 <- rbind(base2, TOTAL = colSums(base2))

        if(summary == TRUE){
                cat('\n============================================')
                cat('\n       RESULTADO DE LOS EJERCICIOS          ')
                cat('\n============================================')
                cat('\n')
                cat('\n')
                print(base2)
        }

        if(intentos == 1){
                notaF <- round((base2[nrow(base2),1]/cant_ej)*100)
        }

        if(intentos > 1){
                intentos <- as.integer(ceiling(intentos))
                detractor <- 0.1
                puntaje <- base2[nrow(base2), 1] - (detractor*(intentos-1))
                puntaje <- ifelse(puntaje < 0, 0, puntaje)
                notaF <- round((puntaje/cant_ej)*100)
        }

        if(isTRUE(enviar)){
                if(is.null(nombre) && is.null(apellido)){
                        stop("El argumento 'nombre' y/o 'apellido' no deben ser NULL si desea enviar su nota final. Complete esos campos y vuelva a correr la funcion.")
                }
                nota <- paste(notaF, "sobre 100")
                nota_escala <- escala(notaF)
                email <- gmailr::mime(
                        To = mail,
                        Bcc = "nicoschlab@gmail.com",
                        From = mail,
                        Subject = paste("Curso IntRo: Resultado de Ejercicio 2 de", nombre, apellido),
                        body = paste("Su nota final del Ejercicio II (Modulo matrices) es:", nota,". En la escala de notas equivale a un:", nota_escala))
                gmailr::send_message(email)
        }

}




