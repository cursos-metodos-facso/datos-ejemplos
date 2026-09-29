library(dplyr)    # Para trabajar con los datos
library(ggplot2)  # Para crear gráficos
library(haven)

url_casen <- "https://bid-ckan.ministeriodesarrollosocial.gob.cl/dataset/105286d9-10a8-410a-b060-a335f3168a46/resource/cacc6ad5-fca1-4476-82f1-b7ea119fae98/download/casen_2024.rdata"  # Dirección oficial de CASEN 2024

load(
  url(url_casen)  # Carga la base directamente desde Internet
)

set.seed(2026) 

# Variables que solo responde un subgrupo (p. ej. ocupados, con ingresos, etc.).
# Exigir casos válidos en ellas define la población desde la cual se muestrea,
# de modo que 481 de las 877 variables queden con los 5000 casos válidos
# (sin filtro solo 300 variables la cumplen).
filtro_validos <- c("o26b", "pobreza_severa", "s26b_1", "y0101", "e6b_no_asiste",
                    "y29_1e", "h5_20", "hh_d_cot_2015", "agnoesc", "o28c", "o32",
                    "v23_sistema", "v24b")

casen_2024 <- casen_2024 %>%
  filter(if_all(all_of(filtro_validos), ~ !is.na(zap_missing(.x)))) %>%
  slice_sample(n = 5000)

save(casen_2024, file = "sub_casen_2024.RData")  # Guarda la base procesada

