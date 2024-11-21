library(tidyverse)

rm(list = ls())
options(scipen = 999)


crear_dataset <- function(seed, n, cor_sig, chi_sig) {
  # Librerías
  library(dplyr)
  
  # Setear semilla
  set.seed(seed)
  
  # Crear variables
  id <- 1:n # Identificador de encuesta (correlativo)
  desigualdad <- sample(1:5, n, replace = TRUE) # Preocupación por la desigualdad (Likert 1 a 5)
  meritocracia <- sample(1:5, n, replace = TRUE) # Preferencia por meritocracia (Likert 1 a 5)
  educ <- sample(1:11, n, replace = TRUE) # Educación (sin NSNR)
  
  # Crear data.frame
  data <- data.frame(id, desigualdad, meritocracia, educ)
  
  # Recodificaciones
  data <- data %>% mutate(
    educ_rec = ifelse(educ >= 8, 1, 0), # 1 = Universitaria o más, 0 = No universitaria
    meritocracia_rec = ifelse(meritocracia <= 2, 0, 1) # Recodificación a dummy
  )
  
  # Ajuste para correlaciones específicas (spearman)
  if (cor_sig) {
    # Ajustar los valores de desigualdad para que haya una correlación con meritocracia
    data <- data %>% mutate(
      desigualdad = case_when(
        meritocracia %in% 1:2 ~ sample(1:3, n, replace = TRUE), # Baja correlación para valores bajos de meritocracia
        meritocracia %in% 3:5 ~ sample(3:5, n, replace = TRUE), # Alta correlación para valores altos de meritocracia
        TRUE ~ desigualdad
      )
    )
  } else {
    # Introducir ruido para romper la correlación
    data <- data %>% mutate(
      desigualdad = sample(1:5, n, replace = TRUE)
    )
  }
  
  # Ajuste para Chi-cuadrado
  if (chi_sig) {
    data <- data %>% mutate(
      educ_rec = if_else(meritocracia >= 3, 1, 0)
    )
  }
  
  # Crear NA's en las cantidades requeridas
  set.seed(seed) # Asegurar reproducibilidad para los NA
  data <- data %>%
    mutate(
      desigualdad = replace(desigualdad, sample(1:n, 10), NA), # 10 casos NA en desigualdad
      meritocracia = replace(meritocracia, sample(1:n, 150), NA), # 150 casos NA en meritocracia
      meritocracia_rec = if_else(is.na(meritocracia), NA, meritocracia_rec) # Emular NA en ingresos_rec según ingresos
    ) %>%
    select(-educ)
  
  return(data)
}

etiquetar <- function(data) {
  # Etiquetar
  data <- data %>% labelled::set_variable_labels(
    desigualdad = "Preocupación por la desigualdad",
    meritocracia = "Preferencia por la meritocracia",
    meritocracia_rec = "Preferencia por la meritocracia recodificado (prefiere o no)",
    educ_rec = "Nivel educacinal recodificado (universitario o no)",
  )
  
  # Etiquetar variables usando sjlabelled::set_labels() para mantenerlas numéricas
  data <- data %>%
    mutate(
      # Etiquetas para educ_rec
      educ_rec = sjlabelled::set_labels(educ_rec,
                                        labels = c(
                                          "No universitaria" = 0,
                                          "Universitaria o más" = 1
                                        )
      ),
      
      # Etiquetas para ingresos_rec
      meritocracia_rec = sjlabelled::set_labels(meritocracia_rec,
                                             labels = c(
                                              "No prefiere" = 0,
                                              "Prefiere" = 1
                                            )
      )
    )
}


# Crear datasets
dataset5 <- crear_dataset(seed = 5, n = 1000, cor_sig = TRUE, chi_sig = FALSE) %>% etiquetar(.)


# Guardar
save(dataset5, file = "dataset5.RData")
