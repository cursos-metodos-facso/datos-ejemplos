library(dplyr)
library(haven)

load("cep_2026.rdata")

# Etiquetas de variable y de valores (basadas en cep-cuest.pdf)
# Los valores se mantienen numéricos con sjlabelled::set_labels()
datos_cep <- datos_cep %>%
  labelled::set_variable_labels(
    educ        = "Nivel educacional",
    mujer       = "Sexo",
    interes_pol = "Interés en la política",
    just_marcha = "Justificación de participar en una marcha"
  ) %>%
  mutate(
    educ = sjlabelled::set_labels(educ, labels = c(
      "Sin estudios formales"                  = 0,
      "Básica incompleta"                      = 1,
      "Básica completa"                        = 2,
      "Media incompleta"                       = 3,
      "Media completa"                         = 4,
      "Técnica (CFT/IP) incompleta"            = 5,
      "Técnica (CFT/IP) completa"              = 6,
      "Universitaria incompleta"               = 7,
      "Universitaria completa"                 = 8,
      "Postgrado incompleto"                   = 9,
      "Postgrado completo"                     = 10
    )),
    mujer = sjlabelled::set_labels(mujer, labels = c(
      "Hombre" = 0,
      "Mujer"  = 1
    )),
    # Invertida respecto al cuestionario (6 - original)
    interes_pol = sjlabelled::set_labels(interes_pol, labels = c(
      "Nada interesado"      = 1,
      "No muy interesado"    = 2,
      "Algo interesado"      = 3,
      "Bastante interesado"  = 4,
      "Muy interesado"       = 5
    )),
    # Invertida respecto al cuestionario (6 - original)
    just_marcha = sjlabelled::set_labels(just_marcha, labels = c(
      "Nunca se justifica"   = 1,
      "2"                    = 2,
      "3"                    = 3,
      "4"                    = 4,
      "Siempre se justifica" = 5
    ))
  )

save(datos_cep, file = "cep_2026_proc.RData")
