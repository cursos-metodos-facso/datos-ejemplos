library(dplyr)    # Para trabajar con los datos
library(ggplot2)  # Para crear gráficos
library(haven)

url_casen <- "https://bid-ckan.ministeriodesarrollosocial.gob.cl/dataset/105286d9-10a8-410a-b060-a335f3168a46/resource/cacc6ad5-fca1-4476-82f1-b7ea119fae98/download/casen_2024.rdata"  # Dirección oficial de CASEN 2024

load(
  url(url_casen)  # Carga la base directamente desde Internet
)

set.seed(2026)

sub_casen_2024 <- casen_2024 |>
  select(
    esc, v12mt, o10, yoprcor, edad) |>
  na.omit() |>
  slice_sample(n = 750)

save(sub_casen_2024, file = "casen_2024.RData")


load("elsoc2019_jv.RData")

elsoc <- elsoc %>%
  select(educ_rec, sexo, ingreso_rec, ingreso_satisfact, jv_cambio)

save(elsoc, file = "elsoc2019_proc.RData")

Justificación violencia por el cambio social
Estudiantes tiren piedras

load("cep_2026.rdata")

datos_cep <- datos_cep %>%
  select(edad, mujer, estatus, conf_carab, conf_gobierno, conf_partidos) %>%
  mutate(estatus_dico = case_when(
    estatus %in% 1:5 ~ 0,
    estatus %in% 6:10 ~ 1
  )) %>%
  rename(sexo = mujer)

save(datos_cep, file = "cep_proc.RData")

load("casen_barrio2024.rdata")

datos_casen <- datos_casen %>%
  select(nivel_educ, edad, mujer, barrio_drogas, barrio_peleas, barrio_ruidos) %>%
  mutate(barrio_peleas_dico = case_when(
    barrio_peleas %in% 1:3 ~ 0,
    barrio_peleas %in% 4:5 ~ 1
  )) %>%
  rename(sexo = mujer)

save(datos_casen, file = "casen_2024_proc.RData")
