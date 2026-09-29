library(dplyr)    # Para trabajar con los datos
library(ggplot2)  # Para crear gráficos
library(haven)

url_casen <- "https://bid-ckan.ministeriodesarrollosocial.gob.cl/dataset/105286d9-10a8-410a-b060-a335f3168a46/resource/cacc6ad5-fca1-4476-82f1-b7ea119fae98/download/casen_2024.rdata"  # Dirección oficial de CASEN 2024

load(
  url(url_casen)  # Carga la base directamente desde Internet
)

set.seed(2026) 

casen_2024 <- casen_2024 %>%
  slice_sample(n = 5000)

save(casen_2024, file = "sub_casen_2024.RData")  # Guarda la base procesada

