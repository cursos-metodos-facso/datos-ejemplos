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
        select(-educ)

    return(data)
}

etiquetar <- function(data) {
    # Etiquetar
    data <- data %>% labelled::set_variable_labels(
        autoritarismo = "Autoritarismo",
        ingresos = "Ingresos",
        educ_rec = "Nivel educacinal recodificado (universitario o no)",
        ingresos_rec = "Ingresos recodificado"
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

# Revisar
sjmisc::frq(dataset1$autoritarismo)
sjmisc::frq(dataset2$autoritarismo)
sjmisc::frq(dataset3$autoritarismo)
sjmisc::frq(dataset4$autoritarismo)

sjmisc::frq(dataset1$ingresos)
sjmisc::frq(dataset2$ingresos)
sjmisc::frq(dataset3$ingresos)
sjmisc::frq(dataset4$ingresos)

sjmisc::frq(dataset1$ingresos_rec)
sjmisc::frq(dataset2$ingresos_rec)
sjmisc::frq(dataset3$ingresos_rec)
sjmisc::frq(dataset4$ingresos_rec)

sjmisc::frq(dataset1$educ_rec)
sjmisc::frq(dataset2$educ_rec)
sjmisc::frq(dataset3$educ_rec)
sjmisc::frq(dataset4$educ_rec)

# Test estadísticos
# dataset1
cor.test(dataset1$ingresos, dataset1$autoritarismo, complete.obs = TRUE, method = "pearson")
chisq.test(dataset1$educ_rec, dataset1$ingresos_rec)
sjPlot::plot_scatter(data = dataset1, x = ingresos, y = autoritarismo)

# dataset2
cor.test(dataset2$ingresos, dataset2$autoritarismo, complete.obs = TRUE, method = "pearson")
chisq.test(dataset2$educ_rec, dataset2$ingresos_rec)
sjPlot::plot_scatter(data = dataset2, x = ingresos, y = autoritarismo)


# dataset3
cor.test(dataset3$ingresos, dataset3$autoritarismo, complete.obs = TRUE, method = "pearson")
chisq.test(dataset3$educ_rec, dataset3$ingresos_rec)
sjPlot::plot_scatter(data = dataset3, x = ingresos, y = autoritarismo)


# dataset4
cor.test(dataset4$ingresos, dataset4$autoritarismo, complete.obs = TRUE, method = "pearson")
chisq.test(dataset4$educ_rec, dataset4$ingresos_rec)
sjPlot::plot_scatter(data = dataset4, x = ingresos, y = autoritarismo)

# Guardar
save(dataset1, file = "dataset1.RData")
save(dataset2, file = "dataset2.RData")
save(dataset3, file = "dataset3.RData")
save(dataset4, file = "dataset4.RData")
