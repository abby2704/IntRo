#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Curso IntRo
# Jerarquia de tipos o coerciones implicitas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tipos <- c("logical", "integer", "double", "character")
tabla <- matrix(0, 4, 4, dimnames = list(tipos, tipos))
for(i in 1:4) for(j in 1:4) {
        tabla[i, j] <- typeof(c(vector(tipos[i]), vector(tipos[j])))
}
noquote(tabla)

