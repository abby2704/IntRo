

base <- read.csv(file.choose())
base <- read.csv("vdem.csv")
vars <- c("country_name",
          "country_text_id",
          "country_id","year",
          "COWcode",
          "v2x_freexp_thick",
          "v2x_frassoc_thick",
          "v2x_suffr",
          "v2xel_frefair",
          "v2x_elecoff",
          "v2xcl_rol",
          "v2x_jucon",
          "v2xlg_legcon",
          "v2xeg_eqprotec",
          "v2xeg_eqaccess",
          "v2xeg_eqdr",
          "v2x_cspart",
          "v2xdd_dd",
          "v2xel_locelec",
          "v2xel_regelec",
          "v2xel_elecparl",
          "v2xlg_leginter",
          "v2xel_elecpres",
          "v2x_hosinter",
          "v2x_hosabort",
          "v2x_legabort",
          "v2xel_elecpres",
          "v2x_pubcorr","v2x_execorr")
base <- base[,vars]
write.csv(base,"v-dem-reducida.csv")


