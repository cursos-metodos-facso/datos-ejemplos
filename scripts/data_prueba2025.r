rm(list = ls())
options(scipen = 999)

# Función para crear dataset con cantidades de NSNR fijas y pasar NSNR a NA
library(tidyverse)

crear_dataset <- function(seed, n, cor_sig, chi_sig) {
    # Librerías
    library(dplyr)

    # Setear semilla
    set.seed(seed)

    # Crear variables
    id <- 1:n # Identificador de encuesta (correlativo)
    autoritarismo <- sample(1:100, n, replace = TRUE) # Autoritarismo
    educ <- sample(1:11, n, replace = TRUE) # Educación (sin NSNR)

    low_mid_vals <- seq(250000, 990000, by = 10000)
    high_vals <- seq(100000, 3000000, by = 10000)

    ingresos <- c(
        sample(low_mid_vals, n - 250, replace = TRUE),
        sample(high_vals, 250, replace = TRUE)
    ) # Ingresos

    # Crear data.frame
    data <- data.frame(id, autoritarismo, educ, ingresos)

    # Recodificaciones
    data <- data %>% mutate(
        educ_rec = ifelse(educ >= 8, 1, 0), # 1 = Universitaria o más, 0 = No universitaria
        ingresos_rec = case_when(
            ingresos %in% 10000:500000 ~ 1, # Bajos ingresos
            ingresos %in% 500001:1000000 ~ 2, # Medios ingresos
            ingresos %in% 1000001:7000000 ~ 3 # Altos ingresos
        )
    )

    # Ajuste para correlaciones específicas (pearson)
    if (cor_sig) {
        # En lugar de asignar valores completamente aleatorios, ajustamos los valores
        # de autoritarismo para que sigan una tendencia lineal con algo de ruido
        data <- data %>% mutate(
            autoritarismo = case_when(
                ingresos %in% 250000:500000 ~ 30 + (ingresos / 100000) * 2 + rnorm(n(), 0, 5), # Ruido controlado
                ingresos %in% 500001:1000000 ~ 50 + (ingresos / 100000) * 0.5 + rnorm(n(), 0, 5), # Ruido controlado
                ingresos %in% 1000000:3000000 ~ 70 + (ingresos / 100000) * 0.2 + rnorm(n(), 0, 5), # Ruido controlado
                TRUE ~ autoritarismo
            ),
            autoritarismo = round(autoritarismo)
        )
    }

    # Ajuste para Chi-cuadrado
    if (chi_sig) {
        data <- data %>% mutate(
            educ_rec = if_else(ingresos > 2000000, 1, 0)
        )
    }

    # Crear NA's en las cantidades requeridas
    set.seed(seed) # Asegurar reproducibilidad para los NA
    data <- data %>%
        mutate(
            autoritarismo = replace(autoritarismo, sample(1:n, 10), NA), # 10 casos NA en autoritarismo
            ingresos = replace(ingresos, sample(1:n, 150), NA), # 150 casos NA en ingresos
            ingresos_rec = if_else(is.na(ingresos), NA, ingresos_rec) # Emular NA en ingresos_rec según ingresos
        ) %>%
        select(-educ, apoyo_dem=autoritarismo, sexo=educ_rec)

    return(data)
}

etiquetar <- function(data) {
    # Etiquetar
    data <- data %>% labelled::set_variable_labels(
        apoyo_dem = "Apoyo a la democracia",
        ingresos = "Ingresos",
        sexo = "Sexo",
        ingresos_rec = "Ingresos recodificado"
    )

    # Etiquetar variables usando sjlabelled::set_labels() para mantenerlas numéricas
    data <- data %>%
        mutate(
            # Etiquetas para educ_rec
          sexo = sjlabelled::set_labels(sexo,
                labels = c(
                    "Hombre" = 0,
                    "Mujer" = 1
                )
            ),

            # Etiquetas para ingresos_rec
            ingresos_rec = sjlabelled::set_labels(ingresos_rec,
                labels = c(
                    "Bajos ingresos" = 1,
                    "Medios ingresos" = 2,
                    "Altos ingresos" = 3
                )
            )
        )
}

# Crear datasets
dataset1 <- crear_dataset(seed = 1, n = 1000, cor_sig = TRUE, chi_sig = FALSE) %>% etiquetar(.)
dataset2 <- crear_dataset(seed = 2, n = 1000, cor_sig = FALSE, chi_sig = TRUE) %>% etiquetar(.)
dataset3 <- crear_dataset(seed = 3, n = 1000, cor_sig = FALSE, chi_sig = FALSE) %>% etiquetar(.)
dataset4 <- crear_dataset(seed = 4, n = 1000, cor_sig = TRUE, chi_sig = TRUE) %>% etiquetar(.)


# Guardar
save(dataset1, file = "dataset1.RData")
save(dataset2, file = "dataset2.RData")
save(dataset3, file = "dataset3.RData")
save(dataset4, file = "dataset4.RData")

#################### DATOS PRUEBA RECUPERATIVA

data_recuperativa <- crear_dataset(seed = 5, n = 1000, cor_sig = FALSE, chi_sig = FALSE)

## 1) Ajustar apoyo_dem para tener correlación MODERADA POSITIVA con ingresos
set.seed(5)

# Índices donde ambas variables no son NA
idx_use <- which(!is.na(data_recuperativa$ingresos) & !is.na(data_recuperativa$apoyo_dem))

# Estandarizar ingresos en esos casos
z_ingresos <- scale(data_recuperativa$ingresos[idx_use])

# Generar nuevo apoyo_dem con relación lineal moderada + ruido
pref_tmp <- 50 + 7 * z_ingresos + rnorm(length(idx_use), mean = 0, sd = 15)
pref_tmp <- round(pmin(pmax(pref_tmp, 1), 100))  # limitar a 1–100

# Reemplazar solo en los casos usados (respetando los NA previos)
data_recuperativa$apoyo_dem[idx_use] <- pref_tmp

## 2) Ajustar sexo (educ_rec) para tener una ASOCIACIÓN MODERADA con ingresos_rec
set.seed(55)

idx_ir <- which(!is.na(data_recuperativa$ingresos_rec))

prob_univ <- dplyr::case_when(
  data_recuperativa$ingresos_rec[idx_ir] == 1 ~ 0.65,  # en bajos ingresos algo más de 1
  data_recuperativa$ingresos_rec[idx_ir] == 2 ~ 0.50,  # en medios ~50/50
  data_recuperativa$ingresos_rec[idx_ir] == 3 ~ 0.35,  # en altos ingresos algo menos de 1
  TRUE ~ 0.5
)

data_recuperativa$sexo[idx_ir] <- rbinom(length(idx_ir), size = 1, prob = prob_univ)

# Etiquetar con la función original (usa nombres apoyo_dem y sexo)
data_recuperativa <- etiquetar(data_recuperativa)

# --------- CAMBIO DE NOMBRES SOLO EN dataset5 ----------
# apoyo_dem -> pref_redis
# sexo      -> universitaria
data_recuperativa <- data_recuperativa %>%
  dplyr::rename(
    pref_redis   = apoyo_dem,
    universitaria = sexo
  )

# Actualizar etiquetas de variable SOLO para dataset5
data_recuperativa <- labelled::set_variable_labels(
  data_recuperativa,
  pref_redis    = "Preferencias redistributivas",
  universitaria = "Educación universitaria (1 = universitaria o más, 0 = no universitaria)"
)

# Actualizar etiquetas de valores para universitaria SOLO en dataset5
data_recuperativa$universitaria <- sjlabelled::set_labels(
  data_recuperativa$universitaria,
  labels = c(
    "No universitaria"       = 0,
    "Universitaria o más"    = 1
  )
)

# Guardar nueva base
save(data_recuperativa, file = "data_recuperativa.RData")
