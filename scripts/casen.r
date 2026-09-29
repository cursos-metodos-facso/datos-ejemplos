library(dplyr)    # Para trabajar con los datos
library(ggplot2)  # Para crear gráficos
library(haven)

url_casen <- "https://bid-ckan.ministeriodesarrollosocial.gob.cl/dataset/105286d9-10a8-410a-b060-a335f3168a46/resource/cacc6ad5-fca1-4476-82f1-b7ea119fae98/download/casen_2024.rdata"  # Dirección oficial de CASEN 2024

load(
  url(url_casen)  # Carga la base directamente desde Internet
)

set.seed(2026)

sub_casen_2024 <- casen_2024 |>
  filter(edad >= 18) |>
  transmute(
    escolaridad     = haven::zap_labels(esc),
    metros_vivienda = haven::zap_labels(v12mt),
    horas_trabajo   = na_if(haven::zap_labels(o10), -88),
    ingreso_laboral = haven::zap_labels(yoprcor),
    edad            = haven::zap_labels(edad)
  ) |>
  na.omit() |>
  slice_sample(n = 750)

save(sub_casen_2024, file = "sub_casen_2024.RData")
