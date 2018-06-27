#' @title Autoevaluacion de 'Ejercicios III' (Modulo data.frame).
#'
#' @description Es una funcion que permite autoevaluar los ejercicios del modulo
#'     de data.frame del curso Introduccion al software estadistico R.
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
#'     correspondientes al modulo de data.frame.
#'
#' @examples
#'
#' df1 <- datasets::iris[,1:2]
#'
#' auto_eval_df(nombre = "Nicolas",
#'                  apellido = "Schmidt",
#'                  mail = NULL,
#'                  enviar = FALSE,
#'                  intentos = 3,
#'                  summary = TRUE)
#' @export


auto_eval_df <- function(nombre = NULL,
                             apellido = NULL,
                             mail = NULL,
                             intentos = 1,
                             enviar = FALSE,
                             summary = TRUE){

        espacio <- ls(search()[1])
        cant_ej <- 8
        if(length(espacio)==0){
                stop("No hay ningun objeto creado en la sesion de trabajo. Puede consultarlo con la funcion 'objects()' o 'ls()' ")
        }
        vector_objetos <- paste0("df", 1:cant_ej)
        faltantes <- vector_objetos %in% espacio
        if(sum(faltantes)==0){
                stop("En el aspacio de trabajo no hay ninguno de los objeto del ejercicio creados. Recuerde que debe utilizar los mombres que figuran en los ejercicios")
        }

        base <- as.list(stats::setNames(rep(NA, cant_ej), paste("Ejercicio", 1:cant_ej)))

        if(vector_objetos[1] %in% espacio){
                if(sum(dim(df1))!=152){
                        warning("Ejercicio 1: La dimension del data.frame no es correcta.")
                }
                if(!is.character(df1[,1])){
                        warning("Ejercicio 1: El tipo de dato de la variable 1 no es correcto. Debe ser de tipo character.")
                }
                if(!is.factor(df1[,2])){
                        warning("Ejercicio 1: El tipo de dato de la variable 2 no es correcto. Debe ser de tipo factor.")
                }
                base[[1]] <- ifelse(sum(dim(df1)) == 152 && is.character(df1[,1]) && is.factor(df1[,2]), 1, 0)
        }
        if(vector_objetos[2] %in% espacio){
                if(sum(dim(df2))!= 52){
                        warning("Ejercicio 2: La dimension del data.frame no es correcta.")
                }
                if(length(levels(df2[,2]))!= 16) {
                        warning("Ejercicio 2: Los niveles del factor no son los correcto. Recuerde que la variable es .")
                }

                base[[2]] <- ifelse(sum(dim(df2))== 52 && length(levels(df2[,2]))== 16, 1, 0)
        }
        if(vector_objetos[3] %in% espacio){
                if(sum(dim(df3))!= 53){
                        warning("Ejercicio 3: La dimension del data.frame no es correcta.")
                }
                if(names(df3)[3]!="speedMM"){
                        warning("Ejercicio 3: El nombre de la tercer variableno es correcto.")
                }
                if(all(abs(df3[,1] - mean(df3[,1])) != df3[,3]))  {
                        warning("Ejercicio 3: El calculo de la variable no es correcto. Deve restar el valor de la media a cada valor. Y ese valor debe estar en valor absoluto")
                }
                base[[3]] <- ifelse(sum(dim(df3))==53 && names(df3)[3]=="speedMM" && all(abs(df3[,1]-mean(df3[,1]))==df3[,3]), 1, 0)
        }
        if(vector_objetos[4] %in% espacio){
                if(sum(dim(df4))!= 53){
                        warning("Ejercicio 4: La dimension del data.frame no es correcta.")
                }
                if(sum(is.na(df4$dist)) != 28){
                        warning("Ejercicio 4: La cantidad de valores NA no es correcta.")
                }
                base[[4]] <- ifelse(sum(dim(df4))== 53 && sum(is.na(df4$dist))==28, 1, 0)
        }
        if(vector_objetos[5] %in% espacio){
                if(sum(dim(df5))!=304){
                        warning("Ejercicio 5: La dimension del data.frame no es correcta.")
                }
                if(!is.integer(df5[,1])){
                        warning("Ejercicio 5: La primer variavle debe contener numeros estrictamente enteros.")
                }
                if(length(levels(df5[,2]))!=5){
                        warning("Ejercicio 5: Los niveles del factor deben ser 5.")
                }
                if(any(nchar(unique(df5[,3])[1:5]) < 4)){
                        warning("Ejercicio 5: Las palabras de esta variable no pueden tener menos de 4 letras.")
                }
                if(!is.logical(df5[,4])){
                        warning("Ejercicio 5: Las palabras de esta variable no pueden tener menos de 4 letras.")
                }
                if(!all(is.na(df5[seq(2, 300, 2),4]))){
                        warning("Ejercicio 5: Los valores NA que debe contener esta variable no son correctos.")
                }
                base[[5]] <- ifelse(sum(dim(df5))==304 &&
                                            is.integer(df5[,1]) &&
                                            any(nchar(unique(df5[,3])[1:5]) > 3) &&
                                            is.logical(df5[,4]) &&
                                            all(is.na(df5[seq(2, 300, 2),4])) , 1, 0)
        }
        if(vector_objetos[6] %in% espacio){
                if(sum(dim(df6))!= 25){
                        warning("Ejercicio 6: La dimension del data.frame no es correcta.")
                }
                if(sum(is.na(df6))!= 0){
                        warning("Ejercicio 6: En el data.frame no debe haber valores NA.")
                }
                base[[6]] <- ifelse(sum(dim(df6))== 53 && sum(is.na(df6))== 0, 1, 0)
        }
        if(vector_objetos[7] %in% espacio){
                if(sum(dim(df7))!= 31){
                        warning("Ejercicio 7: La dimension del data.frame no es correcta.")
                }
                if(!all(names(df7) == paste0("Columna", 1:5))){
                        warning("Ejercicio 7: Los nombres de las variables no son correctos. Verifique haber cargado correctamente los datos")
                }
                if(sum(apply(is.na(df7), 1, sum)==3)!=0){
                        warning("Ejercicio 7: Debe eliminar las observaciones que contienen tres valores NA")
                }
                base[[7]] <- ifelse(sum(dim(df7))== 31 &&
                                            all(names(df7) == paste0("Columna", 1:5)) &&
                                            sum(apply(is.na(df7), 1, sum)==3)==0, 1, 0)
        }
        if(vector_objetos[8] %in% espacio){
                if(any(rownames(df8)==rownames(datasets::mtcars))){
                        warning("Ejercicio 8: Las filas no deben estar nombradas.")
                }
                if(!any(df8[,1]==rownames(datasets::mtcars))){
                        warning("Ejercicio 8: la primer columna debe contener los nombres de las filas de 'mtcars'.")
                }
                base[[8]] <- ifelse(any(rownames(df8)!=rownames(datasets::mtcars)) &&
                                    any(df8[,1]==rownames(datasets::mtcars)), 1, 0)
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
                        Subject = paste("Curso IntRo: Resultado de Ejercicio 3 de", nombre, apellido),
                        body = paste("Su nota final del Ejercicio III (Modulo data.frame) es:", nota,". En la escala de notas equivale a un:", nota_escala))
                gmailr::send_message(email)
        }

}




